(ns org.nfrac.cppnx.webgl-trace
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [org.nfrac.cppnx.webgl-common :refer [hsv2rgb-glsl]]
            [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]))

(def start-cppn
  {:domain :trace
   :inputs #{:bias :t :z}
   :outputs #{:y}
   :nodes {:init :gaussian}
   :edges {:init {:t 1.0
                  :z 1.0
                  :bias 1.0}
           :y {:init 1.0}}})

(def a-t (g/attribute "a_t" :float))

(def a-z (g/attribute "a_z" :float))

(def v-color (g/varying "v_color" :vec4 :highp))

(defn vertex-shader
  [cppn w-exprs]
  (let [in-exprs {:bias 1.0, :t a-t, :z a-z}
        out-exprs (cppnx/build-cppn-vals cppn in-exprs w-exprs)
        z01 (g/+ (g/* a-z 0.5) 0.5)
        col (g/vec4 (hsv2rgb-glsl z01 1 1) 1)]
    {(g/gl-position) (g/vec4 a-t
                             (:y out-exprs)
                             0 1)
     v-color col}))

(def fragment-shader
  {(g/gl-frag-color) v-color})

(defn setup
  [gl state]
  (let [cppn (:cppn state)
        ws (cppnx/cppn-weights cppn)
        w-uniforms (map #(g/uniform (str "u_weight" %) :float) (range (count ws)))
        w-exprs (zipmap (cppnx/edge-list cppn) w-uniforms)
        program (p/program {:vertex-shader (vertex-shader cppn w-exprs)
                            :fragment-shader fragment-shader
                            :precision {:float :highp}})
        vs  (.createShader gl ggl/VERTEX_SHADER)
        fs  (.createShader gl ggl/FRAGMENT_SHADER)
        pgm (.createProgram gl)]
    (doto gl
      (.clearColor 0 0 0 1)
      (.lineWidth 4)
      (.shaderSource vs (-> program :vertex-shader :glsl))
      (.compileShader vs)
      (.shaderSource fs (-> program :fragment-shader :glsl))
      (.compileShader fs)
      (.attachShader pgm vs)
      (.attachShader pgm fs)
      (.linkProgram pgm))
    (when-not (.getProgramParameter gl pgm ggl/LINK_STATUS)
      (println "Shader link failed:" (.getProgramInfoLog gl pgm))
      (println "Vertex shader log:" (.getShaderInfoLog gl vs))
      (println "Fragment shader log:" (.getShaderInfoLog gl fs))
      (println "Vertex shader glsl:")
      (println (-> program :vertex-shader :glsl)))
    {:gl gl
     :gl-program pgm
     :w-uniforms w-uniforms
     :ws ws
     :vertex-buffer (.createBuffer gl)
     :z-buffer (.createBuffer gl)}))

(defn load-weights
  [gl info w-vals]
  (doseq [[unif w-val] (map vector (:w-uniforms info) w-vals)]
    (when-let [loc (.getUniformLocation gl (:gl-program info) (:name unif))]
      (.uniform1f gl loc w-val)))
  gl)

(def t-vals
  (range -1.0 1.0 (/ 1 500)))

(def attr-data
    (for [[i z] (map-indexed vector [-0.67 -0.33 0 0.33 0.67])
          t (if (zero? (mod i 2))
              t-vals
              (reverse t-vals))]
      [z t]))

(def t-data
  (js/Float32Array.
    (map second attr-data)))

(def z-data
  (js/Float32Array.
    (map first attr-data)))

(defn render
  [gl-info w-vals]
  (let [gl (:gl gl-info)
        pgm (:gl-program gl-info)
        buf (:vertex-buffer gl-info)
        zbuf (:z-buffer gl-info)]
    (doto gl
      (.clear (.-COLOR_BUFFER_BIT gl))
      (.bindBuffer ggl/ARRAY_BUFFER buf)
      (.bufferData ggl/ARRAY_BUFFER t-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-t)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-t))
        1 ggl/FLOAT false 0 0)
      (.bindBuffer ggl/ARRAY_BUFFER zbuf)
      (.bufferData ggl/ARRAY_BUFFER z-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-z)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-z))
        1 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (load-weights gl-info w-vals)
      (.drawArrays ggl/LINE_STRIP 0 (.-length t-data)))))

(ns org.nfrac.cppnx.webgl-image
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [org.nfrac.cppnx.webgl-common :refer [hsv2rgb-glsl]]
            [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]))

(def start-cppn
  {:domain :image
   :inputs #{:bias :x :y :d}
   :outputs #{:h :s :v}
   :nodes {:init :gaussian}
   :edges {:init {:d 1.0
                  :y 1.0}
           :h {:init 1.0}
           :s {:init 0.5
               :x -1.0}
           :v {:init 1.0}}})

(def a-position (g/attribute "a_VertexPosition" :vec2))

(def v-position (g/varying "v_position" :vec2 :highp))

(def vertex-shader
  {(g/gl-position) (g/vec4 a-position 0 1)
   v-position a-position})

(defn xy-to-hsv-shader
  [cppn w-exprs]
  (let [x (g/swizzle v-position :x)
        y (g/swizzle v-position :y)
        d (g/- (g/* (g/sqrt (g/+ (g/* x x) (g/* y y)))
                    (/ 2 (Math/sqrt 2.0)))
               1.0)
        in-exprs {:bias 1.0, :x x, :y y, :d d}
        out-exprs (cppnx/build-cppn-vals cppn in-exprs w-exprs)
        ;; for colours, convert [-1 1] to [0 1]
        out-exprs-01 (remap #(g/+ (g/* % 0.5) 0.5) out-exprs)]
    {(g/gl-frag-color) (g/vec4 (hsv2rgb-glsl (:h out-exprs-01)
                                             (:s out-exprs-01)
                                             (:v out-exprs-01)) 1)}))

(defn setup
  [gl state]
  (let [cppn (:cppn state)
        ws (cppnx/cppn-weights cppn)
        w-uniforms (map #(g/uniform (str "u_weight" %) :float) (range (count ws)))
        w-exprs (zipmap (cppnx/edge-list cppn) w-uniforms)
        program (p/program {:vertex-shader vertex-shader
                            :fragment-shader (xy-to-hsv-shader cppn w-exprs)
                            :precision {:float :highp}})
        vs  (.createShader gl ggl/VERTEX_SHADER)
        fs  (.createShader gl ggl/FRAGMENT_SHADER)
        pgm (.createProgram gl)]
    (doto gl
      (.clearColor 0 0 0 1)
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
      (println "Fragment shader glsl:")
      (println (-> program :fragment-shader :glsl)))
    {:gl gl
     :gl-program pgm
     :w-uniforms w-uniforms
     :ws ws
     :vertex-buffer (.createBuffer gl)}))

(defn load-weights
  [gl info w-vals]
  (doseq [[unif w-val] (map vector (:w-uniforms info) w-vals)]
    (when-let [loc (.getUniformLocation gl (:gl-program info) (:name unif))]
      (.uniform1f gl loc w-val)))
  gl)

(defn render
  [gl-info w-vals]
  (let [gl (:gl gl-info)
        pgm (:gl-program gl-info)
        vx-data (js/Float32Array. #js [1 1, -1 1, 1 -1, -1 -1])
        buf (:vertex-buffer gl-info)]
    (doto gl
      (.clear (.-COLOR_BUFFER_BIT gl))
      (.bindBuffer ggl/ARRAY_BUFFER buf)
      (.bufferData ggl/ARRAY_BUFFER vx-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-position)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-position))
        2 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (load-weights gl-info w-vals)
      (.drawArrays ggl/TRIANGLE_STRIP 0 4))))

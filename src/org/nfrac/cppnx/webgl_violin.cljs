(ns org.nfrac.cppnx.webgl-violin
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [org.nfrac.cppnx.webgl-common :refer [hsv2rgb-glsl]]
            [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]))

(def start-cppn
  {:inputs #{:bias :z :d}
   :outputs #{:left :right :h}
   :nodes {:init :gaussian}
   :edges {:init {:z 1.0
                  :d 1.0}
           :left {:init 1.0}
           :right {:init 0.5
                   :bias -0.5}
           :h {:init 0.9}}})

(def a-variate (g/attribute "a_variate" :float))

(def a-vx-index (g/attribute "a_vxindex" :float))

(def v-color (g/varying "v_color" :vec4 :highp))

(defn ->01 [z] (g/+ (g/* z 0.5) 0.5))

(defn vertex-shader
  [cppn w-exprs]
  (let [in-exprs {:bias 1.0, :z a-variate, :d (g/abs a-variate)}
        out-exprs (cppnx/build-cppn-vals cppn in-exprs w-exprs)
        v01 (->01 (:h out-exprs))
        col (g/vec4 (hsv2rgb-glsl v01 1.0 0.5) 1.0)]
    {(g/gl-position) (g/vec4 (g/if (g/== 0 a-vx-index)
                                   (g/- 0 (->01 (:left out-exprs)))
                                   (->01 (:right out-exprs)))
                             a-variate ; z = y
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
     :vindex-buffer (.createBuffer gl)}))

(defn load-weights
  [gl info w-vals]
  (doseq [[unif w-val] (map vector (:w-uniforms info) w-vals)]
    (when-let [loc (.getUniformLocation gl (:gl-program info) (:name unif))]
      (.uniform1f gl loc w-val)))
  gl)

(def variate-vals
  (range -1.0 1.0 (/ 1 800)))

(def vx-data
  (js/Float32Array.
    (for [z variate-vals
          _ (range 2)]
      z)))

(def vi-data
  (js/Float32Array.
    (for [_ variate-vals
          i (range 2)]
      i)))

(defn render
  [gl-info w-vals]
  (let [gl (:gl gl-info)
        pgm (:gl-program gl-info)
        buf (:vertex-buffer gl-info)
        ibuf (:vindex-buffer gl-info)]
    (doto gl
      (.clear (.-COLOR_BUFFER_BIT gl))
      (.bindBuffer ggl/ARRAY_BUFFER buf)
      (.bufferData ggl/ARRAY_BUFFER vx-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-variate)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-variate))
        1 ggl/FLOAT false 0 0)
      (.bindBuffer ggl/ARRAY_BUFFER ibuf)
      (.bufferData ggl/ARRAY_BUFFER vi-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-vx-index)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-vx-index))
        1 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (load-weights gl-info w-vals)
      (.drawArrays ggl/TRIANGLE_STRIP 0 (.-length vx-data)))))

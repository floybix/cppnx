(ns org.nfrac.cppnx.webgl-image
  (:require [org.nfrac.cppnx.core :as cppnx]
            [org.nfrac.cppnx.compile-webgl :as glsl]
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

(def vertex-shader
  "precision highp float;
attribute vec2 a_position;
varying highp vec2 v_position;
void main(void){
  gl_Position = vec4(a_position, 0.0, 1.0);
  v_position = a_position;
}
")

(defn- to-0-1 [expr]
  (str "(" expr " * 0.5 + 0.5)"))

(defn fragment-shader
  [cppn]
  (let [n-weights (count (cppnx/cppn-weights cppn))
        w-exprs (zipmap (cppnx/edge-list cppn)
                        (map #(str "u_w[" % "]") (range n-weights)))
        in-assigns [["x" "v_position.x"]
                    ["y" "v_position.y"]
                    ["d" "xytod(x,y)"]
                    ["bias" "1.0"]]
        cppn-assigns (glsl/cppn-glsl-assigns cppn w-exprs)
        outpart (str "gl_FragColor = vec4(hsv2rgb(vec3(" \newline
                     "  " (to-0-1 "h") "," \newline
                     "  " (to-0-1 "s") "," \newline
                     "  " (to-0-1 "v") ")),1.0);")]
    (str "precision highp float;
varying highp vec2 v_position;
uniform float u_w[" n-weights "];"
         \newline
         glsl/hsv2rgb-glsl
         glsl/node-fns-glsl
         glsl/xytod-glsl
         \newline
         "void main(void){" \newline
         (glsl/assigns->glsl (concat in-assigns cppn-assigns))
         \newline
         outpart
         "}")))

(defn setup
  [gl cppn]
  (let [n-weights (count (cppnx/cppn-weights cppn))
        vs-glsl vertex-shader
        fs-glsl (fragment-shader cppn)
        vs  (.createShader gl ggl/VERTEX_SHADER)
        fs  (.createShader gl ggl/FRAGMENT_SHADER)
        pgm (.createProgram gl)]
    (doto gl
      (.clearColor 0 0 0 1)
      (.shaderSource vs vs-glsl)
      (.compileShader vs)
      (.shaderSource fs fs-glsl)
      (.compileShader fs)
      (.attachShader pgm vs)
      (.attachShader pgm fs)
      (.linkProgram pgm))
    (when-not (.getProgramParameter gl pgm ggl/LINK_STATUS)
      (println "Shader link failed:" (.getProgramInfoLog gl pgm))
      (println "Vertex shader log:" (.getShaderInfoLog gl vs))
      (println "Fragment shader log:" (.getShaderInfoLog gl fs))
      (println "Fragment shader glsl:")
      (println fs-glsl))
    {:domain (:domain cppn)
     :gl gl
     :gl-program pgm
     :vertex-glsl vs-glsl
     :fragment-glsl fs-glsl
     :n-weights n-weights
     :vertex-buffer (.createBuffer gl)}))

(defn load-weights
  [gl info w-vals]
  (when-let [loc (.getUniformLocation gl (:gl-program info) "u_w")]
    (.uniform1fv gl loc (js/Float32Array. (clj->js w-vals))))
  gl)

(def vx-data
  (js/Float32Array. #js [1 1, -1 1, 1 -1, -1 -1]))

(defn render
  [gl-info w-vals]
  (let [gl (:gl gl-info)
        pgm (:gl-program gl-info)
        buf (:vertex-buffer gl-info)]
    (doto gl
      (.clear (.-COLOR_BUFFER_BIT gl))
      (.bindBuffer ggl/ARRAY_BUFFER buf)
      (.bufferData ggl/ARRAY_BUFFER vx-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm "a_position"))
      (.vertexAttribPointer (.getAttribLocation gl pgm "a_position")
        2 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (load-weights gl-info w-vals)
      (.drawArrays ggl/TRIANGLE_STRIP 0 4))))

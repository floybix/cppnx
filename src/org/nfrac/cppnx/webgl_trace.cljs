(ns org.nfrac.cppnx.webgl-trace
  (:require [org.nfrac.cppnx.core :as cppnx]
            [org.nfrac.cppnx.compile-webgl :as glsl]
            [goog.webgl :as ggl]))

(def start-cppn
  {:domain :trace
   :inputs #{:bias :t :z}
   :outputs #{:y}
   :nodes {:i0 :gaussian}
   :edges {:i0 {:z 1.0
                :t 1.0
                :bias 1.0}
           :y {:i0 1.0}}})

(defn- to-0-1 [expr]
  (str "(" expr " * 0.5 + 0.5)"))

(defn vertex-shader
  [cppn]
  (let [n-weights (count (cppnx/cppn-weights cppn))
        w-exprs (zipmap (cppnx/edge-list cppn)
                        (map #(str "u_w[" % "]") (range n-weights)))
        in-assigns [["t" "a_t"]
                    ["z" "a_z"]
                    ["bias" "1.0"]]
        cppn-assigns (glsl/cppn-glsl-assigns cppn w-exprs)
        outpart (str "gl_Position = vec4(t,y,0,1);" \newline
                     "v_color = vec4(hsv2rgb(vec3(" \newline
                     "  " (to-0-1 "z") "," \newline
                     "  " 0.75 "," \newline
                     "  " 0.8 ")),1.0);")]
    (str "precision highp float;
attribute float a_t;
attribute float a_z;
varying highp vec4 v_color;
uniform float u_w[" n-weights "];"
         \newline
         glsl/hsv2rgb-glsl
         glsl/node-fns-glsl
         \newline
         "void main(void){" \newline
         (glsl/assigns->glsl (concat in-assigns cppn-assigns))
         \newline
         outpart
         "}")))

(def fragment-shader
  "precision highp float;
varying highp vec4 v_color;
void main(void){
  gl_FragColor = v_color;
}
")

(defn setup
  [gl cppn]
  (let [n-weights (count (cppnx/cppn-weights cppn))
        vs-glsl (vertex-shader cppn)
        fs-glsl fragment-shader
        vs  (.createShader gl ggl/VERTEX_SHADER)
        fs  (.createShader gl ggl/FRAGMENT_SHADER)
        pgm (.createProgram gl)]
    (doto gl
      (.clearColor 1 1 1 1)
      (.lineWidth 4)
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
      (println "Vertex shader glsl:")
      (println vs-glsl))
    {:domain (:domain cppn)
     :gl gl
     :gl-program pgm
     :vertex-glsl vs-glsl
     :fragment-glsl fs-glsl
     :n-weights n-weights
     :t-buffer (.createBuffer gl)
     :z-buffer (.createBuffer gl)}))

(defn load-weights
  [gl info w-vals]
  (when-let [loc (.getUniformLocation gl (:gl-program info) "u_w")]
    (.uniform1fv gl loc (js/Float32Array. (clj->js w-vals))))
  gl)

(def t-vals
  (range -1.01 1.01 (/ 1 600)))

(defn mirrored [xs] (concat (map - xs) (reverse xs)))

(def attr-data
    (for [[i z] (map-indexed vector (mirrored (range 0.1 1.0 0.1)))
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
        tbuf (:t-buffer gl-info)
        zbuf (:z-buffer gl-info)]
    (doto gl
      (.clear (.-COLOR_BUFFER_BIT gl))
      (.bindBuffer ggl/ARRAY_BUFFER tbuf)
      (.bufferData ggl/ARRAY_BUFFER t-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm "a_t"))
      (.vertexAttribPointer (.getAttribLocation gl pgm "a_t")
        1 ggl/FLOAT false 0 0)
      (.bindBuffer ggl/ARRAY_BUFFER zbuf)
      (.bufferData ggl/ARRAY_BUFFER z-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm "a_z"))
      (.vertexAttribPointer (.getAttribLocation gl pgm "a_z")
        1 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (load-weights gl-info w-vals)
      (.drawArrays ggl/LINE_STRIP 0 (.-length t-data)))))

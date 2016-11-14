(ns org.nfrac.cppnx.webgl-image
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]))

(defn hsv2rgb-glsl
  "from http://stackoverflow.com/a/17897228/202244

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
"
  [h s v]
  (let [K (g/vec4 1 (g/div 2.0 3.0) (g/div 1.0 3.0) 3.0)
        p (-> (g/fract (g/+ (g/vec3 h h h) (g/swizzle K :xyz)))
              (g/* 6.0)
              (g/- (g/swizzle K :www))
              (g/abs))]
    (g/* v
         (g/mix (g/swizzle K :xxx)
                (g/clamp (g/- p (g/swizzle K :xxx)) 0.0 1.0)
                s))))

(def a-position (g/attribute "a_VertexPosition" :vec2))

(def v-position (g/varying "v_position" :vec2 :highp))

(def vertex-shader
  {(g/gl-position) (g/vec4 a-position 0 1)
   v-position a-position})

(def fragment-shader-old
  (let [x (g/swizzle v-position :x)
        y (g/swizzle v-position :y)
        x01 (g/+ (g/* x 0.5) 0.5)
        y01 (g/+ (g/* y 0.5) 0.5)]
    {(g/gl-frag-color) (g/vec4 (hsv2rgb-glsl x01 1 y01) 1)}))

(defn xy-to-hsv-shader
  [cppn]
  (let [x (g/swizzle v-position :x)
        y (g/swizzle v-position :y)
        d (g/- (g/* (g/sqrt (g/+ (g/* x x) (g/* y y)))
                    (/ 2 (Math/sqrt 2.0)))
               1.0)
        in-exprs {:bias 1.0, :x x, :y y, :d d}
        out-exprs (cppnx/build-cppn-vals cppn in-exprs)
        ;; for colours, convert [-1 1] to [0 1]
        out-exprs-01 (remap #(g/+ (g/* % 0.5) 0.5) out-exprs)]
    {(g/gl-frag-color) (g/vec4 (hsv2rgb-glsl (:h out-exprs-01)
                                             (:s out-exprs-01)
                                             (:v out-exprs-01)) 1)}))

(defn setup
  [gl state]
  (let [program (p/program {:vertex-shader vertex-shader
                            :fragment-shader (xy-to-hsv-shader (:cppn state))
                            :precision {:float :mediump}})
        vs  (.createShader gl ggl/VERTEX_SHADER)
        fs  (.createShader gl ggl/FRAGMENT_SHADER)
        pgm (.createProgram gl)]
    (doto gl
      (.shaderSource vs (-> program :vertex-shader :glsl))
      (.compileShader vs)
      (.shaderSource fs (-> program :fragment-shader :glsl))
      (.compileShader fs)
      (.attachShader pgm vs)
      (.attachShader pgm fs)
      (.linkProgram pgm))
    (when true ;-not (.getProgramParameter gl pgm ggl/LINK_STATUS)
      (println "Shader link failed:" (.getProgramInfoLog gl pgm))
      (println "Vertex shader log:" (.getShaderInfoLog gl vs))
      (println "Fragment shader log:" (.getShaderInfoLog gl fs))
      (println "Fragment shader glsl:")
      (println (-> program :fragment-shader :glsl)))
    pgm))

(defn render
  [gl pgm weights]
  (let [vx-data (js/Float32Array. #js [1 1, -1 1, 1 -1, -1 -1])
        buf (.createBuffer gl)]
    (doto gl
      (.bindBuffer ggl/ARRAY_BUFFER buf)
      (.bufferData ggl/ARRAY_BUFFER vx-data ggl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name a-position)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name a-position))
        2 ggl/FLOAT false 0 0)
      (.useProgram pgm)
      (.drawArrays ggl/TRIANGLE_STRIP 0 4))))

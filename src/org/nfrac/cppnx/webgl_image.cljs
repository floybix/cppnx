(ns org.nfrac.cppnx.webgl-image
  (:require [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]
            [org.nfrac.cppnx.util.algo-graph :as graph]))

;;; cppns

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def ex-cppn
  {:inputs [:bias :x :y :d]
   :nodes {3 :gauss
           4 :lin}
   :deps {3 {:y 1.0}
          4 {3 0.5
             :y -1.0}}
   :out-nodes {:h :d
               :s 4
               :v 3}})

(declare v-position)
(declare hsv2rgb-glsl)

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (into (or (empty m) {})
        (map (fn [[k v]] [k (f v)]))
        m))

(defn gaussian-glsl
  "2 * Math.exp(-Math.pow(inputSignal * 2.5, 2)) - 1"
  [z]
  (-> (g/* z 2.5)
      (g/pow 2.0)
      (g/* -1.0)
      (g/exp)
      (g/* 2.0)
      (g/- 1.0)))

(defn build-cppn-vals
  [cppn in-exprs]
  (let [gr (graph/directed-graph (concat (:inputs cppn) (keys (:nodes cppn)))
                                 (remap keys (:deps cppn)))
        sorted-nids (apply concat (rest (graph/dependency-list gr)))
        node-exprs (reduce (fn [m nid]
                             (let [node-type (get-in cppn [:nodes nid])
                                   ideps (get-in cppn [:deps nid])
                                   sum (->> ideps
                                            (map (fn [[from-id w]]
                                                   (g/* w (get m from-id))))
                                            (reduce g/+))
                                   sumw (reduce g/+ (map g/abs (vals ideps)))
                                   expr (case node-type
                                          :sin (g/sin sum)
                                          :lin (g/div sum sumw)
                                          :gauss (gaussian-glsl sum))]
                                (assoc m nid expr)))
                           in-exprs
                           ;; topological sort
                           sorted-nids)]
    (remap node-exprs (:out-nodes cppn))))

(defn xy-to-hsv-shader
  [cppn]
  (let [x (g/swizzle v-position :x)
        y (g/swizzle v-position :y)
        d (g/- (g/* (g/sqrt (g/+ (g/* x x) (g/* y y)))
                    (/ 2 (Math/sqrt 2.0)))
               1.0)
        in-exprs {:bias 1.0, :x x, :y y, :d d}
        out-exprs (build-cppn-vals cppn in-exprs)
        ;; for colours, convert [-1 1] to [0 1]
        out-exprs-01 (remap #(g/+ (g/* % 0.5) 0.5) out-exprs)]
    {(g/gl-frag-color) (g/vec4 (hsv2rgb-glsl (:h out-exprs-01)
                                             (:s out-exprs-01)
                                             (:v out-exprs-01)) 1)}))

;;; webgl

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

(def fragment-shader
  (xy-to-hsv-shader ex-cppn))

(defn setup
  [gl state]
  (let [program (p/program {:vertex-shader vertex-shader
                            :fragment-shader fragment-shader
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
    (when-not (.getProgramParameter gl pgm ggl/LINK_STATUS)
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

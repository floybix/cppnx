(ns org.nfrac.cppnx.webgl-common
  (:require [gamma.api :as g]
            [gamma.program :as p]))

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

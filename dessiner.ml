let dimensions m =
let h = Array.length m in
let l = Array.length m.(0) in
  (l, h);;


let decompose c =
  let b = c mod 256 in
  let v = (c / 256) mod 256 in
  let r = (c / 65536) mod 256 in
(r,v,b);;

let melanger c1 c2 op =
  let (r1,v1,b1) = decompose c1 in
  let (r2,v2,b2) = decompose c2 in
  let mix a b op =
    (op * a + ((255 - op) * b))/255
  in
  let r3 = mix r1 r2 op in
  let v3 = mix v1 v2 op in
  let b3 = mix b1 b2 op in
  Graphics.rgb r3 v3 b3;;


let arraymap2 f ta tb =
let g i b = f ta.(i) b in
Array.mapi g tb;;

let matrixmap2 f =
arraymap2 (arraymap2 f);;

let dessiner_image img x y =
  let (largeur,hauteur) = dimensions img in
  let fond = Graphics.dump_image (Graphics.get_image x y largeur hauteur) in
  let melanger_tordu (c1,o) c2 = melanger c1 c2 o in
  Graphics.draw_image (Graphics.make_image (matrixmap2 melanger_tordu img fond)) x y;;

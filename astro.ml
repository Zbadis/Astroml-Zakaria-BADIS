open Directions;;
module Images = Images50;;
let perso = Images.rebot;;

(* Graphics.close_graph ();; *)
(* Fenetre principale*)
Graphics.open_graph " 500x550";;
Graphics.set_color (Graphics.rgb 255 255 255);;
Graphics.fill_rect 0 0 500 500;

(* La banière de score et autonomie*)
Graphics.set_color (Graphics.rgb 200 173 127);;
Graphics.moveto 50 520;;
Graphics.fill_rect 0 500 500 50;;
Graphics.set_color (Graphics.rgb 255 255 255);;

Graphics.draw_string "Score : 2500 ";;(*Prochainement afficher le score réel*)
Graphics.moveto 300 520;;
Graphics.draw_string "Autonomie : 30 % ";; (*Prochainement afficher l'autonomie réel*)

(*recolorer la fenetre principale *)
Graphics.set_color (Graphics.rgb 0 0 0);;

(*Initialisation du score *)
let score = ref 0;;

(*Initialisation du niveau d'autonomie 50% *)
let autonomie = ref 0;;

(* ************************************** *)
let f i =
  Graphics.draw_segments [|
      (50*i,0,50*i,500);
      (0,50*i,500,50*i)
     |];
in
List.iter f [1;2;3;4;5;6;7;8;9;10];;

Graphics.remember_mode false;;

let afficher img (i,j) =
  Dessiner.dessiner_image img (!j * 50) (300 - !i * 50);;

(* Position des bateries*)
let bateries =  [(ref 6,ref 9);(ref 6,ref 0);(ref (-3),ref 0);(ref (-3),ref 9)];;
(* étoiles*)
let etoiles =  [(ref 1,ref 0);(ref 2,ref 0);(ref 3,ref 0);(ref 4,ref 0);
                 (ref 0,ref 9);(ref 1,ref 9);(ref 2,ref 9);(ref 3,ref 9);
                 (ref (-3),ref 3);(ref (-3),ref 4);
                 (ref 4,ref 4);(ref 5,ref 4)];;
(*Rochers *)
let rochers =  [(ref 1,ref 5);(ref 4,ref 9);(ref 5,ref 8);(ref (-2),ref 8);(ref (-3),ref 2);(ref (-1),ref 0);(ref 6,ref 1);(ref 5,ref 1)];;


(* Position du personnage*)
let perso_i, perso_j = ref 1, ref 4;;

let afficher_decor () =
  Graphics.synchronize();;
let afficher_perso() =
  afficher perso (perso_i,perso_j);;
let afficher_mobiles () =
  List.iter (afficher Images.baterie) bateries;
  List.iter (afficher Images.star) etoiles;
  List.iter (afficher Images.rock) rochers;
  afficher_perso ();;

let deplacer direction =
  let () = match direction with
    | E -> if(!perso_j + 1 < 10) then perso_j := !perso_j + 1
    | O -> if(!perso_j - 1 > -1) then perso_j := !perso_j - 1  
    | S -> if(!perso_i + 1 < 7) then  perso_i := !perso_i + 1
    | N -> if(!perso_i - 1 > -4) then  perso_i := !perso_i - 1
  in
  let ramasser_baterie (bi,bj) =
    if (!bi,!bj) = (!perso_i,!perso_j) then
      begin
        autonomie := !autonomie + 20; 
        bi := 4;
        bj := 5;
      end
  in

(*Ramasser les étoiles *)
  let ramasser_etoile (ei,ej) =
    if (!ei,!ej) = (!perso_i,!perso_j) then
      begin
        score := !score + 1;
        autonomie := !autonomie - 10; 
        ei := 0;
        ej := 4;
      end
  in

 (* Si le joueur touche un rocher il reste a sa place*)
 let toucher_rocher (ri,rj) =
    if (!ri,!rj) = (!perso_i,!perso_j) then
      begin
        match direction with
        | E -> perso_j := !perso_j - 1
    	| O -> perso_j := !perso_j + 1  
    	| S -> perso_i := !perso_i - 1
    	| N -> perso_i := !perso_i + 1
        ;
        autonomie := !autonomie - 10;
      end
  in
  List.iter ramasser_baterie bateries;
  List.iter ramasser_etoile etoiles;
  List.iter toucher_rocher rochers;
  afficher_decor ();
  afficher_mobiles ();;

let jouer p = Interprete.run p deplacer;;


print_string "          *** космонавт ***\n";;
print_string "       Salut jeune astronaute\n";;
print_string "\n";;
(* TODO trouver le nom de l'auteur *)
print_string " Crédit image : lostgarden.com\n";;
afficher_mobiles ();;

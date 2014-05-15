open Directions;;
module Images = Images50;;
let perso = Images.robot;;

(* Graphics.close_graph ();; *)
(* Fenetre principale*)
Graphics.open_graph " 500x550";;
(*Nom de la fênetre *)
Graphics.set_window_title "Astroml-BZ :)";;
Graphics.set_color (Graphics.rgb 255 255 255);;
Graphics.fill_rect 0 0 500 500;

(* La bannière de l'aide*)
Graphics.set_color (Graphics.rgb 200 173 127);;
Graphics.fill_rect 0 500 500 50;;
Graphics.set_color (Graphics.rgb 255 255 255);;
Graphics.moveto 300 520;;
Graphics.draw_string " Bonne partie :-) ";;
Graphics.moveto 20 530;;
Graphics.draw_string "Aide : ramassez toutes les etoiles ";; 
Graphics.moveto 20 515;;
Graphics.draw_string "       sans epuiser votre batterie ";; 

(*recolorer la fenetre principale *)
Graphics.set_color (Graphics.rgb 0 0 0);;

(*Initialisation du score *)
let score = ref 0;;

(*Initialisation du niveau d'autonomie 45% *)
let autonomie = ref 45;;

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

(* Position des batteries*)
let batteries =  [(ref 6,ref 9);(ref 6,ref 0);(ref (-3),ref 0);(ref (-3),ref 9);(ref 1,ref 0);];;
(* étoiles*)
let etoiles =  [(ref (-3),ref 5);(ref 2,ref 0);(ref 3,ref 0);(ref 4,ref 0);
                 (ref 0,ref 9);(ref 1,ref 9);(ref 2,ref 9);(ref 3,ref 9);
                 (ref (-3),ref 3);(ref (-3),ref 4);
                 (ref 4,ref 4);(ref 5,ref 4)];;
(*Rochers *)
let rochers =  [(ref 4,ref 9);(ref 5,ref 8);(ref (-2),ref 8);(ref (-3),ref 2);(ref (-1),ref 0);(ref 6,ref 1);(ref 5,ref 1)];;


(* Position du personnage*)
let perso_i, perso_j = ref 1, ref 4;;

let afficher_decor () =
  Graphics.synchronize();;
let afficher_perso() =
  afficher perso (perso_i,perso_j);;
let afficher_mobiles () =
  List.iter (afficher Images.batterie) batteries;
  List.iter (afficher Images.star) etoiles;
  List.iter (afficher Images.rock) rochers;
  afficher_perso ();;

let deplacer direction =
  let () = match direction with
    | E -> if(!perso_j + 1 < 10) then perso_j := !perso_j + 1  ; autonomie := !autonomie - 5; 
    | O -> if(!perso_j - 1 > -1) then perso_j := !perso_j - 1  ; autonomie := !autonomie - 5; 
    | S -> if(!perso_i + 1 < 7) then  perso_i := !perso_i + 1  ; autonomie := !autonomie - 5; 
    | N -> if(!perso_i - 1 > -4) then  perso_i := !perso_i - 1 ; autonomie := !autonomie - 5; 
  in
  let ramasser_batterie (bi,bj) =
    if (!bi,!bj) = (!perso_i,!perso_j) then
      begin
        autonomie := !autonomie + 50; 
        bi := -15;
        bj := 15;
      end
  in

(*Ramasser les étoiles *)
  let ramasser_etoile (ei,ej) =
    if (!ei,!ej) = (!perso_i,!perso_j) then
      begin
        score := !score + 10;
	ei := -15;
        ej := 15;
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
      end
  in
  List.iter ramasser_batterie batteries;
  List.iter ramasser_etoile etoiles;
  List.iter toucher_rocher rochers;

  (* tester si le niveau d'autonmie n'est pas épuisé  => la partie est fini*)
  if(!autonomie <= 0) then 
			begin 
			print_string "\n\n****** Dommage vous avez pérdu :( ******\n\n";
			print_string "\n\n** La partie est fini, merci d'avoir joué **\n\n";
			Graphics.close_graph ();
			end ;
  (* tester si le joueur a pris toute les étoiles => la partie est fini*)
  if(!score = 120 & !autonomie > 0) then 
			begin 
			print_string "\n\n******* BRAVO VOUS AVEZ GAGNE :) ******\n\n";
			print_string "\n\n** La partie est fini, merci d'avoir joué **\n\n";
			Graphics.close_graph ();
			end ;

  print_string "Votre score : ";
  print_int !score;
  print_string "    Niveau d'autonomie : ";
  print_int !autonomie;
  print_string " %";
  print_string "\n\n";	
  afficher_decor ();
  afficher_mobiles ();;

let jouer p = Interprete.run p deplacer;;


print_string "          *** космонавт ***\n";;
print_string "       Salut jeune astronaute\n";;
print_string "\n";;
(* TODO trouver le nom de l'auteur *)
print_string " Crédit image : lostgarden.com\n";;
afficher_mobiles ();;

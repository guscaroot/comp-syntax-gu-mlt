--# -path=.:../abstract
concrete MicroLangFre_ of MicroLang = open MicroResFre, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : MainPOS => Str }  ;
    VP = {verb : Verb ; compl : Str ; complm : MainPOS } ; ---s special case of Mini
    Comp = Adjective ; --a: AdjAgreement?
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement ; m : MainPOS} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Gender => Str; n: Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS S = { s = S.s ! S.m };
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = { 
        s = \\vp.compl -> case vp.complm of {
        No => np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl ;
        Pr => np.s ! Nom ++ vp.compl ++ vp.verb.s ! agr2vform np.a
        } 
      } ;
  
    UseV v = {
      verb = v ;
      compl = [] ;
      complm = No ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc ; -- NP object in the accusative, preposition first ;
      complm = np.m ; -- TO DO : change the variable name (loc pre/post) and make it vary together with np.m here
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s ! AdjAgr Sg M ; -- TO DO : make agrrement with subject
      complm = No ; -- TO DO : change the variable name (loc pre/post) and fix it to Post here
      } ;
      
    CompAP ap = {s = ap.s };
    --{
      --s = \\n,g => (ap.s ! (AdjAgr n g))
      --} ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
      a = Agr P3 det.n cn.g ;
      m = No ;
      } ;
      
    UsePron p = {
      s = p.s ;
      a = p.a ;
      m = Pr ;
    } ;
            
    a_Det = {s = table { M => "un" ; F => "une" } ; n = Sg} ; --- a/an can get wrong
    aPl_Det = {s = table { M => "des" ; F => "des" } ; n = Pl} ;
    the_Det = {s = table {  M => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "le"} ;
                            F => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "la"}} ; n = Sg} ;
    thePl_Det = {s = table { M => "les" ; F => "les" } ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = \\n => ((cn.s ! n)  ++ ap.s ! (AdjAgr n cn.g)) ;
      g = cn.g
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    he_Pron = {
      s = table {Nom => "il" ; Acc => "le"} ;
      a = Agr P3 Sg M;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Acc => "la"} ;
      a = Agr P3 Sg F ;
      } ;
    they_Pron = {
      s = table {Nom => "ils" ; Acc => "les"} ;
      a = Agr P3 Pl M;
      } ;
    -- they_Pron_F = {
      --s = table {Nom => "elles" ; Acc => "les"} ;
      --a = Agr Pl F;
      --} ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" M;
lin apple_N = mkN "pomme" F;
lin baby_N = mkN "bébé" M;
lin bad_A = mkA "mauvais" ;
lin beer_N = mkN "bière" F;
lin big_A = mkA "grand" ;
lin bike_N = mkN "vélo" M;
lin bird_N = mkN "oiseau" M;
lin black_A = mkA "noir" ;
lin blood_N = mkN "sang" M;
lin blue_A = mkA "bleu" ;
lin boat_N = mkN "bateau" M;
lin book_N = mkN "livre" M;
lin boy_N = mkN "garçon" M ;
lin bread_N = mkN "pain" M;
lin break_V2 = mkV2 (mkV "casser") ;
lin buy_V2 = mkV2 (mkV "acheter") ;
lin car_N = mkN "voiture" F;
lin cat_N = mkN "chat" M;
lin child_N = mkN "enfant" M;
lin city_N = mkN "ville" F;
lin clean_A = mkA "propre" ;
lin clever_A = mkA "intelligent" ;
lin cloud_N = mkN "nuage" M;
lin cold_A = mkA "froid" ;
lin come_V = mkV "venir";
lin computer_N = mkN "ordinateur" M;
lin cow_N = mkN "vache" F;
lin dirty_A = mkA "sale" ;
lin dog_N = mkN "chien" M;
lin drink_V2 = mkV2 (mkV "boire") ;
lin eat_V2 = mkV2 (mkV "manger") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feu" M;
lin fish_N = mkN "poisson" M;
lin flower_N = mkN "fleur" F;
lin friend_N = mkN "ami" M;
lin girl_N = mkN "fille" F;
lin good_A = mkA "bon" ;
lin go_V = mkV "aller" ;
lin grammar_N = mkN "grammaire" F;
lin green_A = mkA "vert" ;
lin heavy_A = mkA "lourd" ;
lin horse_N = mkN "cheval" M;
lin hot_A = mkA "chaud" ;
lin house_N = mkN "maison" M;
-- lin john_PN = mkPN "Jean" ;
lin jump_V = mkV "sauter" ;
lin kill_V2 = mkV2 "tuer" ;
-- lin know_VS = mkVS (mkV "savoir") ;
lin language_N = mkN "langue" F;
lin live_V = mkV "vivre" ;
lin love_V2 = mkV2 (mkV "aimer") ;
lin man_N = mkN "homme" M;
lin milk_N = mkN "lait" M;
lin music_N = mkN "musique" F;
lin new_A = mkA "nouveau" ;
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "vieu" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire") ;
lin ready_A = mkA "prêt" ;
lin red_A = mkA "rouge" ;
lin river_N = mkN "rivière" F;
lin run_V = mkV "courir" ;
lin sea_N = mkN "mer" F;
lin see_V2 = mkV2 (mkV "voir") ;
lin ship_N = mkN "navire" M;
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "petit" ;
lin star_N = mkN "étoile" F;
lin swim_V = mkV "nager" ;
lin teach_V2 = mkV2 (mkV "enseigner") ;
lin train_N = mkN "train" M;
lin travel_V = mkV "voyager" ;
lin tree_N = mkN "arbre" M;
lin understand_V2 = mkV2 (mkV "comprendre") ;
lin wait_V2 = mkV2 "attendre" ;
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" ;
lin water_N = mkN "eau" F;
lin white_A = mkA "blanc" ;
lin wine_N = mkN "vin" M;
lin woman_N = mkN "femme" F;
lin yellow_A = mkA "jaune" ;
lin young_A = mkA "jeune" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Gender -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n,g -> lin N (smartNoun n g) ;
    --mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      --= \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA : Str -> A
    = \s -> lin A (smartAdj s) ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (regVerb s) ;
    --mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      --= \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (regVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (regVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}

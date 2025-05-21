--# -path=.:../abstract
concrete MicroLangFre of MicroLang = open MicroResFre, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : NPAgreement => Str ; complm : MainPOS ; adv: Str} ; ---s special case of Mini. Modified compl as a table to allow Adjective complement to agree with NP subject
    Comp = Adjective ; 
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement ; m : MainPOS} ; -- added parameter m to allow NP object position to vary: Noun -> after Verb but Pronoun -> before Verb
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Gender => Str; n: Number} ; -- modified s as a table to allow agreement of determiner with noun gender
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS S = { s = S.s };
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {  
      s = case vp.complm of { -- variation in NP object position depending on main POS in NP. Also agreeement between adj compl and np subject
        No => np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl ! (NPAgr (agr2number np.a) (agr2gender np.a)) ++ vp.adv;
        Pr => np.s ! Nom ++ vp.compl ! (NPAgr (agr2number np.a) (agr2gender np.a)) ++ vp.verb.s ! agr2vform np.a ++ vp.adv
      } ;
    } ;
      
    UseV v = {
      verb = v ;
      compl = \\a => [] ;
      complm = No ;
      adv = []
    } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\a =>  v2.c ++ np.s ! Acc ; -- NP object in the accusative, preposition first (even if not yet used in French) ;
      complm = np.m ; 
      adv = []
    } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = \\a => comp.s ! a ;
      complm = No ; 
      adv = []
    } ;
      
    CompAP ap = {s = ap.s ; l = ap.l};
      
    AdvVP vp adv = {
      verb = vp.verb ;
      compl = \\ a => vp.compl ! a  ;
      complm = vp.complm ;
      adv = vp.adv ++ adv.s -- added specific parameter adv to allow PredVPS string building to vary with NP object in different locations
    } ;
      
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
            
    a_Det = {
      s = table { 
        M => "un" ;
        F => "une"
      } ; 
      n = Sg
    } ;
    aPl_Det = {
      s = table { 
        M => "des" ; 
        F => "des" 
      } ; 
      n = Pl
    } ;
    the_Det = {
      s = table { 
        M => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "le"} ;
        F => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "la"}
      } ;
      n = Sg
    } ;
    thePl_Det = {
      s = table { 
        M => "les" ;
        F => "les" 
      } ; 
      n = Pl
    } ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = \\n => case ap.l of { -- variation of adjective location relative to the noun depending on adjective
        Pre => (ap.s ! (NPAgr n cn.g) ++ (cn.s ! n)) ;
        Post => ((cn.s ! n) ++ ap.s ! (NPAgr n cn.g))
      };
      g = cn.g
    } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Dat} ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    --i_Pron = { -- P1 Sg pronoun added to make some tests but commented for testing against RGL
      --s = table { Nom => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "j'" ; _ => "je"} ;
                  --Acc => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "m'" ; _ => "me"} ;
                  --Dat => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "m'" ; _ => "me"}} ;
      --a = Agr P1 Sg M;
    --} ;

    --iFem_Pron = { -- P1 Sg pronoun added to make some tests but commented for testing against RGL
      --s = table { Nom => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "j'" ; _ => "je"} ;
                  --Acc => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "m'" ; _ => "me"} ;
                  --Dat => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u" => "m'" ; _ => "me"}} ;
      --a = Agr P1 Sg F;
    --} ;

    he_Pron = {
      s = table { 
        Nom => "il" ;
        Acc => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "le"} ;
        Dat => "lui" -- added case dative to use with prepositions
      } ;
      a = Agr P3 Sg M;
    } ;
    she_Pron = {
      s = table { 
        Nom => "elle" ;
        Acc => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "l'" ; _ => "la"} ;
        Dat => "elle" -- added case dative to use with prepositions
      } ;
      a = Agr P3 Sg F ;
    } ;
    they_Pron = {
      s = table {
        Nom => "ils" ; 
        Acc => "les" ; 
        Dat => "eux" -- added case dative to use with prepositions
      } ;
      a = Agr P3 Pl M;
    } ;
    --theyFem_Pron = { -- P3 Pl F pronoun added to make some tests but commented for testing against RGL
      --s = table {Nom => "elles" ; Acc => "les" ; Dat => "elles"} ;
      --a = Agr P3 Pl F;
      --} ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" M;
lin apple_N = mkN "pomme" F;
lin baby_N = mkN "bébé" M;
lin bad_A = mkA "mauvais" Pre ;
lin beer_N = mkN "bière" F;
lin big_A = mkA "grand" Pre ;
lin bike_N = mkN "vélo" M;
lin bird_N = mkN "oiseau" M;
lin black_A = mkA "noir" Post ;
lin blood_N = mkN "sang" M;
lin blue_A = mkA "bleu" Post ;
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
lin clean_A = mkA "propre" Post ;
lin clever_A = mkA "intelligent" Post ; -- translated as "sage" in RGL but chose "intelligent" as a more general meaning
lin cloud_N = mkN "nuage" M;
lin cold_A = mkA "froid" Post ;
lin come_V = mkV "venir";
lin computer_N = mkN "ordinateur" M;
lin cow_N = mkN "vache" F;
lin dirty_A = mkA "sale" Post ;
lin dog_N = mkN "chien" M;
lin drink_V2 = mkV2 (mkV "boire") ;
lin eat_V2 = mkV2 (mkV "manger") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feu" M;
lin fish_N = mkN "poisson" M;
lin flower_N = mkN "fleur" F;
lin friend_N = mkN "ami" M;
lin girl_N = mkN "fille" F;
lin good_A = mkA "bon" Pre ;
lin go_V = mkV "aller" ;
lin grammar_N = mkN "grammaire" F;
lin green_A = mkA "vert" Post ;
lin heavy_A = mkA "lourd" Post ;
lin horse_N = mkN "cheval" M;
lin hot_A = mkA "chaud" Post ;
lin house_N = mkN "maison" F;
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
lin new_A = mkA "nouveau" Pre ;
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "vieux" Pre ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire") ;
lin ready_A = mkA "prêt" Post ;
lin red_A = mkA "rouge" Post ;
lin river_N = mkN "rivière" F;
lin run_V = mkV "courir" ;
lin sea_N = mkN "mer" F;
lin see_V2 = mkV2 (mkV "voir") ;
lin ship_N = mkN "navire" M; -- translated as "bateau" in RGL but kept "navire" for variation
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "petit" Pre ;
lin star_N = mkN "étoile" F;
lin swim_V = mkV "nager" ;
lin teach_V2 = mkV2 (mkV "enseigner") ;
lin train_N = mkN "train" M;
lin travel_V = mkV "voyager" ;
lin tree_N = mkN "arbre" M;
lin understand_V2 = mkV2 (mkV "comprendre") ;
lin wait_V2 = mkV2 "attendre" ;
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" Post ;
lin water_N = mkN "eau" F;
lin white_A = mkA "blanc" Post ;
lin wine_N = mkN "vin" M;
lin woman_N = mkN "femme" F;
lin yellow_A = mkA "jaune" Post ;
lin young_A = mkA "jeune" Pre ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN : Str -> Gender -> Noun  
      = \n,g -> lin N (smartNoun n g) ;

  mkA : Str -> AdjLoc -> A
    = \s,l -> lin A (smartAdj s l) ;

  mkV : (inf : Str) -> V 
      = \s -> lin V (smartVerb s) ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
   {-  mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (regVerb s ** {c = p}) ; -}
    mkV2 : V -> V2            -- verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    {- mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ; -}
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}

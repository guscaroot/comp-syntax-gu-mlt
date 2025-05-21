resource MicroResFre = open Prelude in {

param
  Person = P1 | P2 | P3 ;
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat;
  Gender = M | F ;

  MainPOS = No | Pr ; -- to handle NP object position in VP

  AdjLoc = Pre | Post ; -- to handle adjective location relative to noun in NP

  NPAgreement = NPAgr Number Gender ;

  Agreement = Agr Person Number Gender; 

  -- all forms of normal Fre verbs for present tense as well as infinitive and participles, although not all yet used in MiniGrammar
  VForm = Inf | PresP1Sg | PresP2Sg | PresP3Sg | PresP1Pl | PresP2Pl | PresP3Pl | PastPart | PresPart ;

oper

  agr2number : Agreement -> Number = \agr -> case agr of { 
    (Agr _ n _) => n 
  } ;

  agr2gender : Agreement-> Gender = \agr -> case agr of {
    (Agr _ _ g) => g
  } ;
    
  Noun : Type = {s : Number => Str; g: Gender} ;

  mkNoun : Str -> Str -> Gender -> Noun = \sg,pl,gender -> {
    s = table {Sg => sg ; Pl => pl};
    g = gender
    } ;

  regNoun : Str -> Gender -> Noun = \sg,gender -> mkNoun sg (sg + "s") gender;

  -- smart paradigm
  smartNoun : Str -> Gender -> Noun = \sg,gender -> case sg of {
    _ + ("au"|"eu"|"ou")      => mkNoun sg (sg + "x") gender;
    _ + ("s"|"x"|"z")         => mkNoun sg sg gender;
    x + ("al"|"ail")          => mkNoun sg (x + "aux") gender;
    _                         => regNoun sg gender
    } ;

  Adjective : Type = { s: NPAgreement => Str ; l: AdjLoc } ; -- added parameter l for adjective location relative to noun in NP

  mkAdj : Str -> Str -> AdjLoc -> Adjective = \m, f, loc -> {
    s = table {
      NPAgr Sg M => case m of { 
        "vieux" => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "vieil" ; _ => "vieux"} ;
        "nouveau" => pre {"a"|"e"|"é"|"ê"|"i"|"o"|"u"|"h" => "nouvel" ; _ => "nouveau"} ;
        _ => m } ;
      NPAgr Sg F => f ;
      NPAgr Pl M => case m of {
        _ + "ieux" => m ;
        _ + "eau" => m + "x" ;
        _ + "s" => m ;
        _ => m + "s" } ;
      NPAgr Pl F => f + "s" 
    } ;
    l = loc
  } ;

  regAdj : Str -> AdjLoc -> Adjective = \m,loc -> mkAdj m (m + "e") loc ;

  smartAdj : Str -> AdjLoc -> Adjective = \m,loc -> case m of {
    x + "eau" => mkAdj m (x + "elle") loc ;
    x + "ieux" => mkAdj m (x + "ieille") loc ;
    x + "c" => mkAdj m (m + "he") loc ;
    _ + "e" => mkAdj m m loc ;
    _ + "n" => mkAdj m (m + "ne") loc ;
    _       => regAdj m loc
  } ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,presp1sg,presp2sg,presp3sg,presp1pl,presp2pl,presp3pl,pastpart,prespart : Str) -> Verb
    = \inf,presp1sg,presp2sg,presp3sg,presp1pl,presp2pl,presp3pl,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresP1Sg => presp1sg ;
      PresP2Sg => presp2sg ;
      PresP3Sg => presp3sg ;
      PresP1Pl => presp1pl ;
      PresP2Pl => presp2pl ;
      PresP3Pl => presp3pl ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  smartVerb : (inf : Str) -> Verb = \inf -> case inf of {
    "aller" => mkVerb inf "vais" "vas" "va" "allons" "allez" "vont" "allé" "allant";
    x + "eter" => mkVerb inf ( x + "ète") ( x + "ètes") ( x + "ète") ( x + "etons") ( x + "etez") ( x + "ètent") ( x + "eté") ( x + "etant");
    x + "ger" => mkVerb inf ( x + "ge") ( x + "ges") ( x + "ge") ( x + "geons") ( x + "gez") ( x + "gent") ( x + "gé") ( x + "geant");
    x + "er" => mkVerb inf ( x + "e") ( x + "es") ( x + "e") ( x + "ons") ( x + "ez") ( x + "ent") ( x + "é") ( x + "ant");
    x + "enir" => mkVerb inf ( x + "iens") ( x + "iens") ( x + "ient") ( x + "enons") ( x + "enez") ( x + "iennent") ( x + "enu") ( x + "enant");
    x + "mir" => mkVerb inf ( x + "s") ( x + "s") ( x + "t") ( x + "mons") ( x + "mez") ( x + "ment") ( x + "mi") ( x + "mant");
    x + "oir" => mkVerb inf ( x + "ois") ( x + "ois") ( x + "oit") ( x + "oyons") ( x + "oyez") ( x + "oient") ( x + "u") ( x + "oyant");
    x + "ir" => mkVerb inf ( x + "s") ( x + "s") ( x + "t") ( x + "ons") ( x + "ez") ( x + "ent") ( x + "u") ( x + "ant");
    x + "oire" => mkVerb inf ( x + "ois") ( x + "ois") ( x + "oit") ( x + "uvons") ( x + "uvez") ( x + "oivent") ( x + "u") ( x + "uvant");
    x + "ivre" => mkVerb inf ( x + "is") ( x + "is") ( x + "it") ( x + "ivons") ( x + "ivez") ( x + "ivent") ( x + "écu") ( x + "ivant");
    x + "ire" => mkVerb inf ( x + "is") ( x + "is") ( x + "it") ( x + "isons") ( x + "isez") ( x + "isent") ( x + "u") ( x + "isant");
    x + "tendre" => mkVerb inf ( x + "tends") ( x + "tends") ( x + "tend") ( x + "tendons") ( x + "tendez") ( x + "tendent") ( x + "tendu") ( x + "tendant");
    x + "endre" => mkVerb inf ( x + "ends") ( x + "ends") ( x + "end") ( x + "enons") ( x + "enez") ( x + "ennent") ( x + "is") ( x + "enant")
  } ;  

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ; 

  be_Verb : Verb = mkVerb "être" "suis" "es" "est" "sommes" "êtes" "sont" "été" "étant" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr P1 Sg M => PresP1Sg ;
    Agr P1 Sg F => PresP1Sg ;
    Agr P1 Pl M => PresP1Pl ;
    Agr P1 Pl F => PresP1Pl ;
    Agr P2 Sg M => PresP2Sg ;
    Agr P2 Sg F => PresP2Sg ;
    Agr P2 Pl M => PresP2Pl ;
    Agr P2 Pl F => PresP2Pl ;
    Agr P3 Sg M => PresP3Sg ;
    Agr P3 Sg F => PresP3Sg ;
    Agr P3 Pl M => PresP3Pl ;
    Agr P3 Pl F => PresP3Pl 
    } ;

}

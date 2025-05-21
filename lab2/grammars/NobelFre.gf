concrete NobelFre of Nobel = LabelsFre ** open
  SyntaxFre,
  ParadigmsFre,
  SymbolicFre,
  IrregFre, -- needed for all three verbs 'naître', 'mourir' and 'recevoir'
  TenseFre -- needed for the French tense 'passé composé'
in {

lincat
  Sentence = S ;
  Name = NP ;
  Date = Adv ;

lin
  BornSentence name country date = 
    mkS presentTense anteriorAnt (mkCl name (mkVP (mkVP born_VP (inAdv country)) date)) ; -- modified Tense and Ant to build the 'passé composé'
    
  AwardSentence name award date =
    mkS presentTense anteriorAnt (mkCl name (mkVP (mkVP get_V2 award) date)) ; -- modified Tense and Ant to build the 'passé composé'
      
  DiedSentence name date =
    mkS presentTense anteriorAnt (mkCl name (mkVP die_VP date)) ; -- modified Tense and Ant to build the 'passé composé'

  
  FemaleName s = mkNP (mkPN s.s feminine) ; -- used for gender agreement of verb in passé composé, building the NP as feminine for female laureates
  MaleName s = symb s ; -- used for gender agreement of verb in passé composé, default NP gender is masculine
  StringName s = symb s ;

  YearDate i = SyntaxFre.mkAdv (mkPrep "en") <symb i : NP> ; -- modified to always get preposition 'en' before the date

  he_Name = he_NP ;
  she_Name = she_NP ;
  they_Name = they_NP ;

oper
  inAdv : Country -> Adv = \c -> SyntaxFre.mkAdv c.prep c.np; -- modified as Country includes both NP and Prep to use 
  born_VP = mkVP naître_V ; -- use of irregular verb
  die_VP = mkVP mourir_V ; -- use of irregular verb
  get_V2 = recevoir_V2 ; -- use of irregular verb
 
}
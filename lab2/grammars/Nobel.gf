abstract Nobel = Labels ** {

flags startcat = Sentence ;

cat
  Sentence ;
  Name ;
  Date ;

fun
  BornSentence : Name -> Country -> Date -> Sentence ;
  AwardSentence : Name -> Award -> Date -> Sentence ;
  DiedSentence : Name -> Date -> Sentence ;
  FemaleName : String -> Name ; -- needed for gender agreement
  MaleName : String -> Name ; -- needed for gender agreement
  StringName : String -> Name ;
  YearDate : Int -> Date ;
  he_Name, she_Name, they_Name : Name ;

}

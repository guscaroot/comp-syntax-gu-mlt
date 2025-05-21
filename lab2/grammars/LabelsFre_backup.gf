concrete LabelsFre of Labels = open SyntaxFre, ParadigmsFre, CommonRomance in { -- added CommonRomance to define the gender

lincat Country = {np: NP ; prep: Prep} ;
lincat Award = NP ;

oper mkCountry = overload {
  --mkCountry: N -> Num -> NP = \cn,s -> mkNP (mkPN (mkN cn s)) ; -- added to handle compound nouns and get both gender and number
  --mkCountry : Str -> NP = \s -> mkNP (mkPN s) ;
  --mkCountry : Str -> CommonRomance.Gender -> NP = \s,g -> mkNP (mkPN s g) ; -- added gender when deviant from the simple rule, to allow variation of preposition in building inAdv
  --mkCountry : NP -> NP = \np -> np ;

  mkCountry : Str -> Country = \s -> lin Country  {  -- prep varies with the gender/last letter in string and first letter in string - regular case
    np = mkNP (mkPN s) ;
    prep = case s of {
      _ + "e"                         => mkPrep "en" ; -- gender is feminine
      ("A"|"E"|"É"|"I"|"O"|"U") + _   => mkPrep "en" ; -- gender is masculine and first letter is a vowel
      _                               => mkPrep "au"   -- gender is masculine and first letter is a consonant
    }
  } ;
  mkCountry : Str -> Prep -> Country = \s,p -> lin Country { -- could be the worst case for deviant cases
    np = mkNP (mkPN s) ;
    prep = p
  } ;
  mkCountry : NP -> Prep -> Country = \np,p -> lin Country {  -- \np,p -> { np = mkNP thePl_Det (mkPN s) ; prep = dative }
    np = np ;
    prep = p
  };
  } ;

oper mkAward = overload {
  mkAward : Str -> CommonRomance.Gender -> NP = \s,g -> mkNP the_Det (mkN s g) ; -- added gender to allow agreement of the_Det with the noun ('prix')
  mkAward : NP -> NP = \np -> np ;
  } ;


lin Q800_Costa_Rica_Country = mkCountry "Costa Rica" ; 
lin Q219060_State_of_Palestine_Country = mkCountry (mkNP the_Det (mkN "État de Palestine")) (mkPrep "dans") ; -- prep 'dans' for a state
lin Q37_Lithuania_Country = mkCountry "Lituanie" ; 
lin Q137816_Taiwan_under_Japanese_rule_Country = mkCountry "Taïwan sous domination japonaise" dative; -- prep 'à' for an island
lin Q1028_Morocco_Country = mkCountry "Maroc" ; 
lin Q796_Iraq_Country = mkCountry "Irak" ; 
lin Q184_Belarus_Country = mkCountry "Biélorussie" ; 
lin Q225_Bosnia_and_Herzegovina_Country = mkCountry "Bosnie-Herzégovine" ; 
lin Q20_Norway_Country = mkCountry "Norvège" ; 
lin Q211_Latvia_Country = mkCountry "Lettonie" ; 
lin Q117_Ghana_Country = mkCountry "Ghana" ; 
lin Q39_Switzerland_Country = mkCountry "Suisse" ; 
lin Q159631_Kingdom_of_Württemberg_Country = mkCountry (mkNP the_Det (mkN "Royaume du Wurtemberg")) (mkPrep "dans") ; -- prep "dans" for an historical kingdom
lin Q17_Japan_Country = mkCountry "Japon" ; 
lin Q189_Iceland_Country = mkCountry "Islande" ; 
lin Q221_North_Macedonia_Country = mkCountry "Macédoine du Nord" (mkPrep "en"); -- 'Macédoine' is feminine
lin Q9683_Tang_dynasty_Country = mkCountry "dynastie Tang" (mkPrep "en") ; -- 'dynastie' is feminine
lin Q79_Egypt_Country = mkCountry "Égypte" ; 
lin Q408_Australia_Country = mkCountry "Australie" ; 
lin Q4628_Faroe_Islands_Country = mkCountry (mkNP thePl_Det (mkN "îles Féroé")) dative ; --prep "aux" for island in plural form
lin Q145_United_Kingdom_Country = mkCountry "Royaume-Uni" ; 
lin Q214_Slovakia_Country = mkCountry "Slovaquie" ; 
lin Q16_Canada_Country = mkCountry "Canada" ; 
lin Q924_Tanzania_Country = mkCountry "Tanzanie" ; 
lin Q55502_Kingdom_of_Jerusalem_Country = mkCountry (mkNP the_Det (mkN "Royaume de Jérusalem")) (mkPrep "dans") ; -- prep "dans" for an historical kingdom
lin Q183_Germany_Country = mkCountry "Allemagne" ; 
lin Q754_Trinidad_and_Tobago_Country = mkCountry "Trinité-et-Tobago" dative ; -- prep 'à' for an island
lin Q298_Chile_Country = mkCountry "Chili" ; 
lin Q41_Greece_Country = mkCountry "Grèce" ; 
lin Q30623_Manchukuo_Country = mkCountry "Mandchoukouo" ; 
lin Q774_Guatemala_Country = mkCountry "Guatemala" ; 
lin Q836_Myanmar_Country = mkCountry "Birmanie" ; 
lin Q902_Bangladesh_Country = mkCountry "Bangladesh" ; 
lin Q215_Slovenia_Country = mkCountry "Slovénie" ; 
lin Q7313_Yuan_dynasty_Country = mkCountry (mkNP the_Det (mkN "dynastie Yuan en Chine")) (mkPrep "sous")  ; -- prep "sous" for dynasty
lin Q822_Lebanon_Country = mkCountry "Liban" ; 
lin Q12548_Holy_Roman_Empire_Country = mkCountry (mkNP the_Det (mkN "Saint-Empire romain germanique" Masc)) (mkPrep "dans") ; --prep "dans" for empire
lin Q12407080_early_Islamic_period_in_Palestine_Country = mkCountry (mkNP the_Det (mkN "période islamique précoce en Palestine")) (mkPrep "durant")  ; --no label in French in Wikidata
lin Q717_Venezuela_Country = mkCountry "Venezuela" ; 
lin Q31_Belgium_Country = mkCountry "Belgique" ; 
lin Q794_Iran_Country = mkCountry "Iran" ; 
lin Q43_Turkey_Country = mkCountry "Turquie" ; 
lin Q948_Tunisia_Country = mkCountry "Tunisie" ; 
lin Q258_South_Africa_Country = mkCountry "Afrique du Sud" (mkPrep "en") ; -- Afrique is feminine
lin Q28_Hungary_Country = mkCountry "Hongrie" ; 
lin Q80061_Nobel_Prize_in_Physiology_or_Medicine_Award = mkAward "prix Nobel de physiologie ou médecine" Masc; 
lin Q142_France_Country = mkCountry "France" ; 
lin Q805_Yemen_Country = mkCountry "Yémen" ; 
lin Q881_Vietnam_Country = mkCountry "Viêt Nam" ; 
lin Q7462_Song_dynasty_Country = mkCountry "dynastie Song" (mkPrep "en") ; -- 'dynastie' is feminine 
lin Q12544_Byzantine_Empire_Country = mkCountry (mkNP the_Det (mkN "Empire byzantin")) (mkPrep "dans") ; --prep "dans" for empire
lin Q664_New_Zealand_Country = mkCountry "Nouvelle-Zélande" ; 
lin Q33_Finland_Country = mkCountry "Finlande" ; 
lin Q282428_Mamluk_Sultanate_Country = mkCountry (mkNP the_Det (mkN "sultanat mamelouk d'Égypte" Masc)) (mkPrep "dans") ; --prep "dans" for sultanate
lin Q38104_Nobel_Prize_in_Physics_Award = mkAward "prix Nobel de physique" Masc; 
lin Q9903_Ming_dynasty_Country = mkCountry "dynastie Ming" (mkPrep "en") ; -- 'dynastie' is feminine 
lin Q739_Colombia_Country = mkCountry "Colombie" ; 
lin Q13426199_Republic_of_China_Country = mkCountry (mkNP the_Det (mkN "république de Chine")) (mkPrep "dans") ; --prep "dans" for republic
lin Q55_Netherlands_Country = mkCountry (mkNP thePl_Det (mkN ("Pays-Bas"))) dative ; 
lin Q159_Russia_Country = mkCountry "Russie" ; 
lin Q27_Ireland_Country = mkCountry "Irlande" ; 
lin Q48685_Kingdom_of_Judah_Country = mkCountry (mkNP the_Det (mkN "Royaume de Juda")) (mkPrep "dans") ; -- prep "dans" for an historical kingdom
lin Q810_Jordan_Country = mkCountry "Jordanie" ; 
lin Q36_Poland_Country = mkCountry "Pologne" ; 
lin Q1014_Liberia_Country = mkCountry "Liberia" ; 
lin Q38872_Prussia_Country = mkCountry "Prusse" ; 
lin 'Q574_Timor-Leste_Country' = mkCountry "Timor oriental" ; 
lin Q974_Democratic_Republic_of_the_Congo_Country = mkCountry (mkNP the_Det (mkN "République démocratique du Congo" Fem)) (mkPrep "dans") ; --prep "dans" for republic
lin Q15843470_Roman_Palestine_Country = mkCountry "Palestine romaine" ; 
lin Q40_Austria_Country = mkCountry "Autriche" ; 
lin Q928_Philippines_Country = mkCountry (mkNP thePl_Det (mkN "Philippines" Fem)) dative; -- prep 'aux' for plural
lin Q148_People's_Republic_of_China_Country = mkCountry (mkNP the_Det (mkN "république populaire de Chine")) (mkPrep "dans") ; --prep "dans" for republic
lin Q35_Denmark_Country = mkCountry "Danemark" ; 
lin Q954_Zimbabwe_Country = mkCountry "Zimbabwe" (mkPrep "au"); 
lin Q216173_Free_City_of_Danzig_Country = mkCountry (mkNP the_Det (mkN "Ville libre de Dantzig" Fem)) (mkPrep "dans") ; --prep "dans" when "ville" included
lin Q227_Azerbaijan_Country = mkCountry "Azerbaïdjan" ; 
lin Q252_Indonesia_Country = mkCountry "Indonésie" ; 
lin Q801_Israel_Country = mkCountry "Israël" ; 
lin Q155_Brazil_Country = mkCountry "Brésil" ; 
lin Q29_Spain_Country = mkCountry "Espagne" ; 
lin Q7075820_Occupied_Enemy_Territory_Administration_Country = mkCountry (mkNP thePl_Det (mkN "Territoires ennemis occupés")) (mkPrep "dans") ; --prep "dans" for territoires
lin Q2685298_Romanian_People's_Republic_Country = mkCountry (mkNP the_Det (mkN "République populaire roumaine")) (mkPrep "dans") ; --prep "dans" for republic
lin Q45_Portugal_Country = mkCountry "Portugal" ; 
lin Q32_Luxembourg_Country = mkCountry "Luxembourg" ; 
lin Q115_Ethiopia_Country = mkCountry "Éthiopie" ; 
lin Q193714_Mandatory_Palestine_Country = mkCountry "Palestine mandataire" ; 
lin Q34_Sweden_Country = mkCountry "Suède" ; 
lin Q262_Algeria_Country = mkCountry "Algérie" ; 
lin Q37922_Nobel_Prize_in_Literature_Award = mkAward "prix Nobel de littérature" Masc ; 
lin Q843_Pakistan_Country = mkCountry "Pakistan" ; 
lin Q35637_Nobel_Peace_Prize_Award = mkAward "prix Nobel de la paix" Masc ; 
lin Q1033_Nigeria_Country = mkCountry "Nigeria" ; 
lin Q38_Italy_Country = mkCountry "Italie" ; 
lin Q668_India_Country = mkCountry "Inde" ; 
lin Q496922_Hasmonean_dynasty_Country = mkCountry (mkNP thePl_Det (mkN ("Hasmonéens"))) dative; -- prep 'aux' for plural NOT EXACTLY CORRECT
lin Q212_Ukraine_Country = mkCountry "Ukraine" ; 
lin Q44585_Nobel_Prize_in_Chemistry_Award = mkAward "prix Nobel de chimie" Masc ; 
lin Q760_Saint_Lucia_Country = mkCountry "Sainte-Lucie" dative ; -- prep 'à' for an island
lin Q414_Argentina_Country = mkCountry "Argentine" ; 
lin Q218_Romania_Country = mkCountry "Roumanie" ; 
lin Q213_Czech_Republic_Country = mkCountry "Tchéquie" ; 
lin Q219_Bulgaria_Country = mkCountry "Bulgarie" ; 
lin Q12560_Ottoman_Empire_Country = mkCountry (mkNP the_Det (mkN "Empire ottoman")) (mkPrep "dans") ; --prep "dans" for empire
lin Q224_Croatia_Country = mkCountry "Croatie" ; 
lin Q419_Peru_Country = mkCountry "Pérou" ; 
lin Q1019_Madagascar_Country = mkCountry "Madagascar" dative ; -- prep 'à' for an island 
lin Q30_United_States_Country = mkCountry (mkNP thePl_Det (mkN ("États-Unis"))) dative; -- prep 'aux' for plural 
lin Q180114_Ayyubid_dynasty_Country = mkCountry (mkNP thePl_Det (mkN ("Ayyoubides"))) dative; -- prep 'aux' for plural NOT EXACTLY CORRECT
lin Q8733_Qing_dynasty_Country = mkCountry "dynastie Qing" (mkPrep "en") ; -- 'dynastie' is feminine 
lin Q96_Mexico_Country = mkCountry "Mexique" (mkPrep "au") ; -- masculine despite the final 'e'
lin Q884_South_Korea_Country = mkCountry "Corée du Sud" (mkPrep "en"); -- Corée is feminine
lin Q114_Kenya_Country = mkCountry "Kenya" ; 

}
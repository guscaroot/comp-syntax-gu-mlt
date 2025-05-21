concrete LabelsFre of Labels = open SyntaxFre, ParadigmsFre in {

lincat Country = {np: NP ; prep: Prep} ; -- prep used before country varies with gender, number and 'type' of country (state, dynasty, territory, island...)
lincat Award = NP ;

oper mkCountry = overload {
  
  mkCountry : Str -> Country = \s -> lin Country  {
    -- NP building varies with gender/last letter, first letter, number and type of country (ex: 'Taïwan','le Royaume de Juda', 'les Pays-Bas', 'Suède', 'le Japon')
    np = case s of {
      -- particular cases:
      "Yémen" => mkNP (mkPN s) ; -- added to handle the incorrectly built 'à l'Yémen' from prep dative + article 'le', should be 'au Yémen'
      ("Taïwan"+_|"Trinité"+_|"Sainte-Lucie"|"Madagascar") => mkNP (mkPN s) ; -- country is an island, name is singular, keeping country name without article (ex: 'Madagascar')
      (_+"Empire"+_|"État de"+_|"Royaume "+_|"sultanat"+_) => mkNP the_Det (mkN s masculine) ; -- country is an empire, a state, a sultanate, or an historical kingdom, adding masculine article to country name (ex: 'l'Empire ottoman' / 'le Royaume de Juda')
      ("république"+_|"République"+_|"Ville"+_|"dynastie"+_) => mkNP the_Det (mkN s feminine) ; -- country is an historical republic, dynasty or includes "Ville", adding feminine article to country name (ex: 'la république de Chine')
      -- irregular cases for gender:
      ("Macédoine"+_|"Afrique"+_|"Corée"+_) => mkNP (mkPN s feminine) ; -- gender is feminine despite last letter not being 'e', keeping country name without article (ex: 'Macédoine du Nord')
      ("Zimbabwe"|"Mexique") => mkNP the_Det (mkN s masculine) ; -- gender is masculine despite last letter being 'e', adding article to country name (ex: 'le Mexique')
      -- particular and regular cases, plural: 
      ("Territoires"+_|"Pays-Bas"|"États-Unis") => mkNP thePl_Det (mkN s s masculine) ; -- number is plural and gender is masculine, adding article to country name (ex: 'les Pays-Bas')
      ("îles"+_|"Philippines") => mkNP thePl_Det (mkN s s feminine) ; -- number is plural and gender is feminine, adding article to country name (ex: 'les Philippines')
      -- regular cases, singular:
      _ + "e"                         => mkNP (mkPN s) ; -- number is singular and gender is feminine, keeping country name without article (ex: 'France')
      ("A"|"E"|"É"|"I"|"O"|"U") + _   => mkNP (mkPN s) ; -- number is singular, gender is masculine and first letter is a vowel, keeping country name without article (ex: 'Iran')
      _                               => mkNP the_Det (mkN s) -- number is singular, gender is masculine and first letter is a consonant, adding article to country name (ex: 'le Japon')
    };
    -- prep varies with gender/last letter, first letter, number and type of country (ex: 'à Taïwan','dans le Royaume de Juda', 'aux Pays-Bas', 'en Suède', 'au Japon')
    prep = case s of { -- note: dative = Prep 'à'
      -- particular cases:
      "Yémen" => mkPrep "au" ; -- added to handle the incorrectly built 'à l'Yémen' from prep dative + article 'le', should be 'au Yémen'
      ("Taïwan"+_|"Trinité"+_|"Sainte-Lucie"|"Madagascar") => dative ; -- country is an island, name is singular (ex: 'à Madagascar')
      (_+"Empire"+_|"État de"+_|"Royaume "+_|"sultanat"+_|"république"+_|"République"+_|"Ville"+_|"Territoires"+_) => mkPrep "dans" ; -- country is an empire, a state, a sultanate, an hitorical kingdom or republic, or includes "Ville"/"Territoires" (ex: 'dans l'Empire ottoman')
      ("dynastie"+_) => mkPrep "sous" ; -- country is a dynasty in China (ex: 'sous la dynastie Ming en Chine')
      -- irregular cases for gender:
      ("Macédoine"+_|"Afrique"+_|"Corée"+_) => mkPrep "en" ; -- gender is feminine despite last letter not being 'e' (ex: 'en Macédoine du Nord')
      ("Zimbabwe"|"Mexique") => dative ; -- gender is masculine despite last letter being 'e' (ex: 'au Mexique', contracted form for 'à le Mexique')
      -- regular cases, plural:
      ("Pays-Bas"|"États-Unis"|"îles"+_|"Philippines") => dative ; -- number is plural (ex: 'aux Pays-Bas', contracted form for 'à les Pays-Bas')
      -- regular cases, singular:
      _ + "e"                         => mkPrep "en" ; -- number is singular and gender is feminine (ex: 'en France')
      ("A"|"E"|"É"|"I"|"O"|"U") + _   => mkPrep "en" ; -- number is singular, gender is masculine and first letter is a vowel (ex: 'en Iran')
      _                               => dative  -- number is singular, gender is masculine and first letter is a consonant (ex: 'au Japon', contracted form for 'à le Japon')
    }
  } ;
  mkCountry : Str -> Prep -> Country = \s,p -> lin Country { -- could be for deviant cases but not used
    np = mkNP (mkPN s) ;
    prep = p
  } ;
  mkCountry : NP -> Prep -> Country = \np,p -> lin Country {  -- worst case for deviant cases
    np = np ;
    prep = p
  };
};

oper mkAward = overload {
  mkAward : Str -> Gender -> NP = \s,g -> mkNP the_Det (mkN s g) ; -- added gender to allow agreement of the_Det with the noun ('prix' is masculine)
  mkAward : NP -> NP = \np -> np ;
  } ;


lin Q800_Costa_Rica_Country = mkCountry "Costa Rica" ; 
lin Q219060_State_of_Palestine_Country = mkCountry "État de Palestine" ;
lin Q37_Lithuania_Country = mkCountry "Lituanie" ; 
lin Q137816_Taiwan_under_Japanese_rule_Country = mkCountry "Taïwan sous domination japonaise";
lin Q1028_Morocco_Country = mkCountry "Maroc" ; 
lin Q796_Iraq_Country = mkCountry "Irak" ; 
lin Q184_Belarus_Country = mkCountry "Biélorussie" ; 
lin Q225_Bosnia_and_Herzegovina_Country = mkCountry "Bosnie-Herzégovine" ; 
lin Q20_Norway_Country = mkCountry "Norvège" ; 
lin Q211_Latvia_Country = mkCountry "Lettonie" ; 
lin Q117_Ghana_Country = mkCountry "Ghana" ; 
lin Q39_Switzerland_Country = mkCountry "Suisse" ; 
lin Q159631_Kingdom_of_Württemberg_Country = mkCountry "Royaume du Wurtemberg" ;
lin Q17_Japan_Country = mkCountry "Japon" ; 
lin Q189_Iceland_Country = mkCountry "Islande" ; 
lin Q221_North_Macedonia_Country = mkCountry "Macédoine du Nord" ;
lin Q9683_Tang_dynasty_Country = mkCountry "dynastie Tang en Chine" ; -- modified the label in French to match Q7313 and build "sous la dynastie Tang en Chine"
lin Q79_Egypt_Country = mkCountry "Égypte" ; 
lin Q408_Australia_Country = mkCountry "Australie" ; 
lin Q4628_Faroe_Islands_Country = mkCountry "îles Féroé" ;
lin Q145_United_Kingdom_Country = mkCountry "Royaume-Uni" ; -- country name is established, actual and masculine -> prep 'au' = 'à le' even if "Royaume"
lin Q214_Slovakia_Country = mkCountry "Slovaquie" ; 
lin Q16_Canada_Country = mkCountry "Canada" ; 
lin Q924_Tanzania_Country = mkCountry "Tanzanie" ; 
lin Q55502_Kingdom_of_Jerusalem_Country = mkCountry "Royaume de Jérusalem" ;
lin Q183_Germany_Country = mkCountry "Allemagne" ; 
lin Q754_Trinidad_and_Tobago_Country = mkCountry "Trinité-et-Tobago" ;
lin Q298_Chile_Country = mkCountry "Chili" ; 
lin Q41_Greece_Country = mkCountry "Grèce" ; 
lin Q30623_Manchukuo_Country = mkCountry "Mandchoukouo" ; 
lin Q774_Guatemala_Country = mkCountry "Guatemala" ; 
lin Q836_Myanmar_Country = mkCountry "Birmanie" ; 
lin Q902_Bangladesh_Country = mkCountry "Bangladesh" ; 
lin Q215_Slovenia_Country = mkCountry "Slovénie" ; 
lin Q7313_Yuan_dynasty_Country = mkCountry "dynastie Yuan en Chine" ;
lin Q822_Lebanon_Country = mkCountry "Liban" ; 
lin Q12548_Holy_Roman_Empire_Country = mkCountry "Saint-Empire romain germanique" ;
lin Q12407080_early_Islamic_period_in_Palestine_Country = mkCountry (mkNP the_Det (mkN "période islamique précoce en Palestine")) (mkPrep "durant")  ; -- no label in French in Wikidata
lin Q717_Venezuela_Country = mkCountry "Venezuela" ; 
lin Q31_Belgium_Country = mkCountry "Belgique" ; 
lin Q794_Iran_Country = mkCountry "Iran" ; 
lin Q43_Turkey_Country = mkCountry "Turquie" ; 
lin Q948_Tunisia_Country = mkCountry "Tunisie" ; 
lin Q258_South_Africa_Country = mkCountry "Afrique du Sud" ;
lin Q28_Hungary_Country = mkCountry "Hongrie" ; 
lin Q80061_Nobel_Prize_in_Physiology_or_Medicine_Award = mkAward "prix Nobel de physiologie ou médecine" masculine; 
lin Q142_France_Country = mkCountry "France" ; 
lin Q805_Yemen_Country = mkCountry "Yémen" ; 
lin Q881_Vietnam_Country = mkCountry "Viêt Nam" ; 
lin Q7462_Song_dynasty_Country = mkCountry "dynastie Song en Chine" ; -- modified the label in French to match Q7313 and build "sous la dynastie Song en Chine"
lin Q12544_Byzantine_Empire_Country = mkCountry "Empire byzantin" ;
lin Q664_New_Zealand_Country = mkCountry "Nouvelle-Zélande" ; 
lin Q33_Finland_Country = mkCountry "Finlande" ; 
lin Q282428_Mamluk_Sultanate_Country = mkCountry "sultanat mamelouk d'Égypte" ;
lin Q38104_Nobel_Prize_in_Physics_Award = mkAward "prix Nobel de physique" masculine; 
lin Q9903_Ming_dynasty_Country = mkCountry "dynastie Ming en Chine" ; -- modified the label in French to match Q7313 and build "sous la dynastie Ming en Chine"
lin Q739_Colombia_Country = mkCountry "Colombie" ; 
lin Q13426199_Republic_of_China_Country = mkCountry "république de Chine" ;
lin Q55_Netherlands_Country = mkCountry "Pays-Bas";
lin Q159_Russia_Country = mkCountry "Russie" ; 
lin Q27_Ireland_Country = mkCountry "Irlande" ; 
lin Q48685_Kingdom_of_Judah_Country = mkCountry "Royaume de Juda" ;
lin Q810_Jordan_Country = mkCountry "Jordanie" ; 
lin Q36_Poland_Country = mkCountry "Pologne" ; 
lin Q1014_Liberia_Country = mkCountry "Liberia" ; 
lin Q38872_Prussia_Country = mkCountry "Prusse" ; 
lin 'Q574_Timor-Leste_Country' = mkCountry "Timor oriental" ; 
lin Q974_Democratic_Republic_of_the_Congo_Country = mkCountry (mkNP (mkPN "République démocratique du Congo" feminine)) (mkPrep "en") ; -- country name is established, actual and feminine -> prep 'en' even if "république"
lin Q15843470_Roman_Palestine_Country = mkCountry "Palestine romaine" ; 
lin Q40_Austria_Country = mkCountry "Autriche" ; 
lin Q928_Philippines_Country = mkCountry "Philippines" ;
lin Q148_People's_Republic_of_China_Country = mkCountry (mkNP (mkPN "république populaire de Chine" feminine)) (mkPrep "en") ; -- country name is established, actual and feminine -> prep 'en' even if "république"
lin Q35_Denmark_Country = mkCountry "Danemark" ; 
lin Q954_Zimbabwe_Country = mkCountry "Zimbabwe" ; 
lin Q216173_Free_City_of_Danzig_Country = mkCountry "Ville libre de Dantzig" ;
lin Q227_Azerbaijan_Country = mkCountry "Azerbaïdjan" ; 
lin Q252_Indonesia_Country = mkCountry "Indonésie" ; 
lin Q801_Israel_Country = mkCountry "Israël" ; 
lin Q155_Brazil_Country = mkCountry "Brésil" ; 
lin Q29_Spain_Country = mkCountry "Espagne" ; 
lin Q7075820_Occupied_Enemy_Territory_Administration_Country = mkCountry "Territoires ennemis occupés" ;
lin Q2685298_Romanian_People's_Republic_Country = mkCountry "République populaire roumaine" ;
lin Q45_Portugal_Country = mkCountry "Portugal" ; 
lin Q32_Luxembourg_Country = mkCountry "Luxembourg" ; 
lin Q115_Ethiopia_Country = mkCountry "Éthiopie" ; 
lin Q193714_Mandatory_Palestine_Country = mkCountry "Palestine mandataire" ; 
lin Q34_Sweden_Country = mkCountry "Suède" ; 
lin Q262_Algeria_Country = mkCountry "Algérie" ; 
lin Q37922_Nobel_Prize_in_Literature_Award = mkAward "prix Nobel de littérature" masculine ; 
lin Q843_Pakistan_Country = mkCountry "Pakistan" ; 
lin Q35637_Nobel_Peace_Prize_Award = mkAward "prix Nobel de la paix" masculine ; 
lin Q1033_Nigeria_Country = mkCountry "Nigeria" ; 
lin Q38_Italy_Country = mkCountry "Italie" ; 
lin Q668_India_Country = mkCountry "Inde" ; 
lin Q496922_Hasmonean_dynasty_Country = mkCountry "Royaume hasmonéen" ; -- modified the label in French to build "dans le Royaume hasmonéen" which better expresses a location ("Hasmonéens" is a dynasty and relates more to a period)
lin Q212_Ukraine_Country = mkCountry "Ukraine" ; 
lin Q44585_Nobel_Prize_in_Chemistry_Award = mkAward "prix Nobel de chimie" masculine ; 
lin Q760_Saint_Lucia_Country = mkCountry "Sainte-Lucie" ;
lin Q414_Argentina_Country = mkCountry "Argentine" ; 
lin Q218_Romania_Country = mkCountry "Roumanie" ; 
lin Q213_Czech_Republic_Country = mkCountry "Tchéquie" ; 
lin Q219_Bulgaria_Country = mkCountry "Bulgarie" ; 
lin Q12560_Ottoman_Empire_Country = mkCountry "Empire ottoman" ;
lin Q224_Croatia_Country = mkCountry "Croatie" ; 
lin Q419_Peru_Country = mkCountry "Pérou" ; 
lin Q1019_Madagascar_Country = mkCountry "Madagascar" ; 
lin Q30_United_States_Country = mkCountry "États-Unis" ; 
lin Q180114_Ayyubid_dynasty_Country = mkCountry "sultanat ayyoubide" ; -- modified the label in French to build "dans le sultanat ayyoubide" which better expresses a location ("Ayyoubides" is a dynasty and relates more to a period)
lin Q8733_Qing_dynasty_Country = mkCountry "dynastie Qing en Chine" ; -- modified the label in French to match Q7313 and build "sous la dynastie Qing en Chine" 
lin Q96_Mexico_Country = mkCountry "Mexique" ;
lin Q884_South_Korea_Country = mkCountry "Corée du Sud" ;
lin Q114_Kenya_Country = mkCountry "Kenya" ; 

}
import Comsoc.Interface (evaluate)

main = evaluate $ unlines 
  [ "Müller > Meier > Schulze" 
  , "Meier > Müller > Schulze"
  , "Meier > Schulze > Müller"
  , "Müller > Schulze > Meier"
  , "Schulze > Meier > Müller"
  ]

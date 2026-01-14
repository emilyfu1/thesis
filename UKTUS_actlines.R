personal_care_actlines = c(
  ## --- Personal care, rest, eating ---
  0,        # Unspecified personal care
  110, 111, # Sleep; Sleep in bed not asleep
  210,      # Eating
  5310)     # Resting - time out

restrict_actlines = c(  
  
  ## --- Social life & visits ---
  5120,     # Visiting and receiving visitors
  5130,     # Celebrations
  5190,     # Other specified social life
  5100,     # Unspecified social life
  5000,     # Unspecified social life and entertainment
  
  ## --- Media: TV, video, radio, music ---
  8210, 8211, 8219,   # TV watching
  8220, 8221, 8222, 8229, # Video watching
  8300, 8310, 8311, 8312, 8319, # Radio & music
  8000,     # Unspecified mass media
  
  ## --- Arts, culture, hobbies ---
  5200,     # Unspecified entertainment & culture
  5210,     # Cinema
  5220, 5221, 5222, 5223, 5224, 5225, 5229, # Theatre, concerts, live music
  5230,     # Art exhibitions and museums
  5290, 5291, 5292, 5293, 5294, 5295, 5299, # Cultural visits
  7100, 7110, 7111, 7112, 7119, # Visual arts
  7120, 7121, 7129, # Performing arts
  7130, 7140, # Literary & other arts
  7150, 7160, # Hobbies, collecting
  7190,     # Other specified arts and hobbies
  
  ## --- Games ---
  7300, 7310, 7320, 7321, 7322, 7329, 7330, 7390, # Games & play
  
  ## --- Sports & physical exercise ---
  6000,     # Unspecified sports & outdoor activities
  6100,     # Unspecified physical exercise
  6110, 6111, 6119, # Walking & hiking
  6120,     # Jogging and running
  6130, 6131, 6132, # Biking, skiing, skating
  6140, 6141, 6142, 6143, 6144, 6149, # Ball games
  6150,     # Gymnastics
  6160,     # Fitness
  6170, 6171, 6179, # Water sports
  6190,     # Other physical exercise
  6310, 6311, # Sports-related activities
  
  ## --- Computing & online leisure ---
  7230, 7231, 7239, # Information searching
  7240, 7241, 7249, 7250, 7251, 7259, # Online communication
  7000)      # Unspecified hobbies, games & computing

leisure_actlines = c(personal_care_actlines, restrict_actlines)
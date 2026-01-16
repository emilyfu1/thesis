# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# directories
data_direct = paste0(wd,"/uktus_data/")
uktus_2000_direct = paste0(data_direct,"UKDA-4504-stata8_se/stata8_se/")
uktus_2015_direct = paste0(data_direct,"UKDA-8128-stata/stata/stata11_se/")

# dict define region codes to merge with regional wealth
region_codes = new.env()
region_codes$"NORTH EAST" = 1
region_codes$"NORTH WEST" = 2
region_codes$"YORKSHIRE AND THE HUMBER" = 3
region_codes$"EAST MIDLANDS" = 4
region_codes$"WEST MIDLANDS" = 5
region_codes$"EAST" = 6
region_codes$"EASTERN" = 6
region_codes$"LONDON" = 7
region_codes$"SOUTH EAST" = 8
region_codes$"SOUTH WEST" = 10
region_codes$"WALES" = 11
region_codes$"SCOTLAND" = 12
region_codes$"NORTHERN IRELAND" = 13
region_names = ls(region_codes)
region_map = unlist(as.list(region_codes), use.names = TRUE)

# activity codes

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

childcare_actlines = c(
  
  ## --- Direct childcare ---
  4100,  # Unspecified childcare
  4110,  # Physical care of children
  4120,  # Teaching, reading, helping with homework
  4130,  # Playing with children
  4140,  # Talking with children
  4150,  # Accompanying children
  4160,  # Medical care of children
  
  ## --- Childcare as help ---
  4170,  # Childcare as help
  4171,  # Physical care of children as help
  4172,  # Teaching/helping with homework as help
  4173,  # Playing with children as help
  4174,  # Talking with children as help
  4175   # Accompanying children as help
)

leisure_actlines = c(personal_care_actlines, restrict_actlines)
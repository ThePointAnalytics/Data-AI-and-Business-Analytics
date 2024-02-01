#remove objects 
rm(list =ls(all = T))

#set working environment 
setwd("E:/Archives/The Point Sets")

#libraries 
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)
library(splitstackshape)
library(lubridate)
library(geomtextpath)
library(reactable)
library(reactablefmtr)
library(reactable)
library(htmlwidgets)
library(webshot)

#add fonts
font_add_google("Quattrocento Sans", "raleway")
showtext_auto()

#get the fonts 
font_family1 = 'Quattrocento Sans'
font_family2 = 'raleway'

highlight_color = "#28331b"
highlight_color2 = "#131613"
highlight_color3 = "gray12"
highlight_color4 = "#eeb41c"

caption_label = 'Data Visualizations by Point Analytics\n Data Source:Job Adverts in Kenya\n Email: datapowereddecisons@gmail.com \n X: @_PointAnalytics Instagram:_pointanalytics'


#read the general dataset
df = readr::read_csv("data/research-data-analysis-2024-01-29.csv") 

#get the dimensions 
generated_dim = dim(df)[[1]]

#read the company profiles data
company = readr::read_csv("data/company profiles.csv") %>% 
  dplyr::rename(company_name = title) %>% 
  dplyr::distinct_at("company_name", .keep_all = T)


#combine the datasets 
company_name = df %>% 
  dplyr::left_join(company, by = c('company_link'='company_url'))

#get the top companies
co = company_name %>% 
  dplyr::filter(!is.na(company_name)) %>% 
  dplyr::mutate(company = company_name, 
                company = str_remove_all(company, "Recruitment")) %>% 
  splitstackshape::cSplit("company", "(") %>% 
  dplyr::rename(company =company_1) %>% 
  dplyr::select(-company_2) %>% 
  dplyr::mutate(company_type = case_when(company_type == "Government"| company_type == "Public"~"Government/Public", 
                                 TRUE~company_type), 
                company = case_when(company == "African Population And Health Research Center"~"African Population and Health Research Center", 
                                    TRUE~company),
                company = str_trim(company),
                company_type = case_when(company == "Sama"|company == "Wasoko"|company == "Samuel Hall"|
                                           company == "Founders Factory Africa (FFA)"|
                                           company == "Pharmaceutical Product Development (PPD)"|
                                           company == "Inkomoko"|company == "Majorel Kenya"|
                                           company == "TELUS International"|
                                           company == "South Pole"|company == "Chipper Cash"~"Private", 
                                    company == "The Ministry Labour and Social Protection"|
                                      company == "The National Treasury and Economic Planning"|
                                      company == "Ajira Digital Program"|company == "Universities Fund (UF)"|
                                      company == "Salaries and Remuneration Commission"|
                                      company == "Kenya Nuclear Regulatory Authority ( KNRA)"|
                                      company == "Mama Ngina University College (MNUC)"|
                                      company == "Ministry of Environment, Climate Change and Forestry"|
                                      company == "Nairobi Metropolitan Area Transport Authority"|
                                      company == "National Government Affirmative Action Fund"~"Government/Public", 
                                    company == "United Nations Development Programme (UNDP)"|
                                      company == "International Food Policy Research Institute (IFPRI)"|
                                      company == "United Nations Office for Disaster Risk Reduction"~"NGO",
                                    TRUE~company_type), 
                founded = case_when(founded == "N / A"~"", 
                                    TRUE~founded)) %>% 
  dplyr::mutate(company_type = case_when(company == "UN-Habitat"|company == "World Vision Kenya"|
                                           company == "International Potato Center"|company == "Mercy Corps"|
                                           company == "World Food Programme"|
                                           company == "icipe - African Insect Science for Food and Health"|
                                           company == "Danish Refugee Council"|
                                           company == "Save the Children"|
                                           company =="International Rescue Committee"|
                                           company == "LVCT Health"~"NGO",
                                         TRUE~company_type)) %>% 
  dplyr::group_by(type = company_type, 
    company) %>% 
  dplyr::summarise(company_name = paste(unique(company_name), collapse = ","), 
                   logo = paste(unique(logo), collapse = ","),
                   industry = paste(unique(industry), collapse = ","), 
                   founded = paste(unique(founded), collapse = ","), 
                   type = paste(unique(company_type), collapse = ","),
                   count = n()) %>% 
  dplyr::arrange_at("count", desc) %>% 
  dplyr::mutate(percent = count/sum(count)) %>% 
  dplyr::slice_head(n= 10) %>% 
  dplyr::filter(type != "N / A") %>%  
  dplyr::select(type, logo, company, industry, founded, count) %>% 
  dplyr::mutate(logo = case_when(company =="NCBA Group"~"https://www.african-markets.com/images/markets/nse/NCBA.jpg", 
                                 company == "Save the Children"~"https://www.developmentaid.org/files/organizationLogos/save-the-children-kenya-101850.jpg", 
                                 company == "African Population and Health Research Center"~"https://www.iied.org/sites/default/files/styles/page/public/logos/2023-09/APHRC-Logo-notext.png",
                                 company == "Equity Bank Kenya"~"https://seeklogo.com/images/E/equity-bank-logo-DEE4B9266D-seeklogo.com.png",
                                 company == "International Livestock Research Institute"~"https://www.agrisource.org/medias/919f2aec-f5d6-43bd-e9e6-5c6c50ca2398.png", 
                                 company == "Kenya Revenue Authority"~"https://www.kra.go.ke/images/logo.png",
                                 company == "KCB Bank Kenya"~"https://images.africanfinancials.com/tz-kcb-logo-200x200.png",
                                 company == "University of Nairobi"~"https://www.uonbi.ac.ke/sites/default/files/UoN_Logo.png",
                                 company == "Kenya Institute for Public Policy Research and Analysis"~"https://www.greeneconomycoalition.org/assets/images/logos/KIPPRA-LOGO-Thinking-Policy-Together-3.png",
                                 company == "County Government of Kakamega"~"https://seeklogo.com/images/K/kakamega-county-logo-2A6BCBBA4C-seeklogo.com.png",
                                 company == "KEMRI Wellcome Trust Research Programme"~"https://i0.wp.com/www.tenderyetu.com/wp-content/uploads/2021/08/KEMRI-Wellcome-Trust-Research-Programme-TENDER-2021.png",
                                 company == "Mercy Corps"~"https://www.urban-response.org/sites/alnap/files/styles/grid-4-mobile-logo/public/content/organisation/logos/2018-03-27/mc.jpg",
                                 company == "International Potato Center"~"https://storage.googleapis.com/cgiarorg/2018/02/6b7d81dc-logo-cip-png.png", 
                                 company == "World Food Programme"~"https://upload.wikimedia.org/wikipedia/commons/thumb/5/59/World_Food_Programme_Logo_Simple.svg/240px-World_Food_Programme_Logo_Simple.svg.png",
                                 company == "Hand in Hand Eastern Africa"~"https://www.srmhub.com/media/uploads/business-logo/5770_0HhY4eN.png",
                                 company == "Precision Agriculture for Development"~"https://precisiondev.org/wp-content/uploads/2021/05/PxD-logo.svg",
                                 company == "Aga Khan University"~"https://static.the.akdn/53832/1652455141-aku.png",
                                 company == "Corporate Staffing"~"https://eadn-wc04-12090623.nxedge.io/wp-content/uploads/2018/03/Corporate-Staffing-Logo.jpg", 
                                 company == "One Acre Fund"~"https://static.wixstatic.com/media/a95210_2a8fb496f7f847a996a30c850853539b~mv2.jpg/v1/fit/w_1000%2Ch_1000%2Cal_c%2Cq_80,enc_auto/file.jpg",
                                 TRUE~logo))

#NGO
co %>%
  dplyr::filter(type == "NGO") %>%
  dplyr::mutate(type = case_when(type == "NGO"~"Non-Government Organization (NGO)", 
                                 TRUE~type)) %>% 
  reactable(.,
            theme = fivethirtyeight(centered = TRUE,
                                    header_font_size = 10, 
                                    header_font_color = "#715a48",
                                    font_color = '#896d23'
            ),
            defaultSorted = "type",
            rowStyle = group_border_sort("type"),
            columns = list(
              type = colDef(
                name = "Company Type",
                style = group_merge_sort("type")
              ), 
              logo = colDef(name = '', minWidth = 100, 
                            cell = embed_img(
                              height = 42, 
                              width = 45
                            )),
              count = colDef(
                name = "Number of Job Postings", 
                cell = data_bars(
                  data = .,
                  fill_color = 'white',
                  background = '#e2ebf4',
                  border_style = 'solid',
                  border_width = '1px',
                  border_color = '#896d23',
                  box_shadow = TRUE,
                  text_position = 'outside-end',
                  text_color = "#896d23",
                  number_fmt = scales::comma)))) %>% 
  add_title(
    title = reactablefmtr::html("Non-Government Organizations (NGO) Recruiting for Data, Business Analysis and AI Talent in Kenya"),
    margin = reactablefmtr::margin(t=5,r=0,b=2,l=0), 
    font_weight = "normal",
    font_size = 20, 
    # hjust = 0.5, 
    font_color = "#715a48")




#public
co %>%
  dplyr::filter(type == "Government/Public") %>%
  reactable(.,
            theme = fivethirtyeight(centered = TRUE,
                                    header_font_size = 10, 
                                    header_font_color = "#715a48",
                                    font_color = '#896d23'
            ),
            defaultSorted = "type",
            rowStyle = group_border_sort("type"),
            columns = list(
              type = colDef(
                name = "Company Type",
                style = group_merge_sort("type")
              ), 
              logo = colDef(name = '', minWidth = 100, 
                            cell = embed_img(
                              height = 42, 
                              width = 45
                            )),
              count = colDef(
                name = "Number of Job Postings", 
                cell = data_bars(
                  data = .,
                  fill_color = 'white',
                  background = '#e2ebf4',
                  border_style = 'solid',
                  border_width = '1px',
                  border_color = '#896d23',
                  box_shadow = TRUE,
                  text_position = 'outside-end',
                  text_color = "#896d23",
                  number_fmt = scales::comma)))) %>% 
  add_title(
    title = reactablefmtr::html("Public and Gorvenment Owned Companies Recruiting for Data, Business Analysis and AI Talent in Kenya"),
    margin = reactablefmtr::margin(t=5,r=0,b=2,l=0), 
    font_weight = "normal",
    font_size = 20, 
    # hjust = 0.5, 
    font_color = "#715a48")


co %>%
  dplyr::filter(type == "Private") %>%
  reactable(.,
            theme = fivethirtyeight(centered = TRUE,
                                    header_font_size = 10, 
                                    header_font_color = "#715a48",
                                    font_color = '#896d23'
                                    ),
            defaultSorted = "type",
            rowStyle = group_border_sort("type"),
            columns = list(
              type = colDef(
                name = "Company Type",
                style = group_merge_sort("type")
              ), 
              logo = colDef(name = '', minWidth = 100, 
                                    cell = embed_img(
                                      height = 45, 
                                      width = 60
                                    )),
              count = colDef(
                name = "Number of Job Postings", 
                cell = data_bars(
                  data = .,
                  fill_color = 'white',
                  background = '#e2ebf4',
                  border_style = 'solid',
                  border_width = '1px',
                  border_color = '#896d23',
                  box_shadow = TRUE,
                  text_position = 'outside-end',
                  text_color = "#896d23",
                  number_fmt = scales::comma)))) %>% 
  add_title(
    title = reactablefmtr::html("Private Owned Companies Recruiting for Data, Business Analysis and AI Talent in Kenya"),
    margin = reactablefmtr::margin(t=5,r=0,b=2,l=0), 
    font_weight = "normal",
    font_size = 20, 
    # hjust = 0.5, 
    font_color = "#715a48") %>% 
  add_source(
    source = 'Data: USDA, #TidyTuesday Week 2, 2022',
    margin = reactablefmtr::margin(t=7,r=0,b=0,l=0),
    font_style = "italic"
  )


# Save the reactable table as a PNG file
# Save the reactable table as a PNG file
# saveWidget(x, "analysis images/private owned.html")
# webshot("analysis images/private owned.html", "analysis images/private owned.png", vwidth=1200,vheight=700, delay = 2)




            
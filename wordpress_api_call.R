


#rjsai/
setwd("C:/Users/rsaito/Dropbox/Data Science/Just-R-Things")

devtools::install_github("hrbrmstr/pressur")
pacman::p_load(pressur, httr, anytime, crayon, purrr, stringi)

#https://developer.wordpress.com/docs/oauth2/




#urn:ietf:wg:oauth:2.0:oob
#http://localhost
#urn:ietf:wg:oauth:2.0:oob:auto

wordpress_app <- oauth_app(
  appname = "wordpress",
  key = "56635",
  secret = "kKpqXlEDldsZf0Jwr65adm2J3HC2JN9FOGzxxJeBS2mfL0VpCx679IUoQqtsh9TX",
  redirect_uri = "http://localhost:1410/"
)

wordpress_endpoint <- oauth_endpoint(
  base_url = "https://public-api.wordpress.com/oauth2",
  request = "authenticate",
  authorize = "authorize",
  access = "token"
)

token <- oauth2.0_token(wordpress_endpoint, wordpress_app,
  user_params = list(grant_type = "authorization_code",
  response_type = "code", scope = "global"), cache = TRUE
)

res <- httr::GET(url = sprintf("https://public-api.wordpress.com/rest/v1.1/me"), 
                 token, httr::accept_json())

httr::stop_for_status(res)
me = httr::content(res)


res <- httr::GET(url = sprintf("https://public-api.wordpress.com/rest/v1.1/sites/%s/stats", 
                             me$primary_blog), token, accept_json())
httr::stop_for_status(res)
stats <- httr::content(res)
stats$visits <- purrr::map_df(stats$visits$data, ~purrr::set_names(.x, 
                                                                     stats$visits$fields))
stats$visits$period <- anytime::anydate(stats$visits$period)





res <- httr::GET(url = sprintf("https://public-api.wordpress.com/rest/v1.1/sites/%s/stats/country-views", 
                               me$primary_blog), token, accept_json())
countries <- httr::content(res)
countries$visits <- purrr::map_df(countries$visits$data, ~purrr::set_names(.x, 
                                                                   countries$visits$fields))
                                                                   




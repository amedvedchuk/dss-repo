library(httr)

processGitRepoInfo <- function() {
    
    # 1. Find OAuth settings for github:
    #    http://developer.github.com/v3/oauth/
    oauth_endpoints("github")
    
    # 2. Register an application at https://github.com/settings/applications;
    #    Use any URL you would like for the homepage URL (http://github.com is fine)
    #    and http://localhost:1410 as the callback url
    #
    #    Insert your client ID and secret below - if secret is omitted, it will
    #    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
    myapp <- oauth_app("github", "f8ba456835e9904ec899", "b0dc889328e812c671fc6d9f133dc9fc18a0d5af")
    
    # 3. Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
    
    # 4. Use API
    gtoken <- config(token = github_token)
    
    req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
    stop_for_status(req)
    cont <- content(req)
    
    data <- fromJSON(toJSON(cont))
    
    row <- data[data$name == "datasharing",]
    
    creation_date <- row$created_at
    
    creation_date
    
    # OR:
    # req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
    # stop_for_status(req)
    # content(req)
    
}
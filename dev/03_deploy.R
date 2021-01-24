# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
devtools::check_rhub()
devtools::check_win_devel()
rhub::check_for_cran()

# no git
# cd c:/Users/Bruno/Documents/pacote/geobox
# git add .
# git commit --no-verify
# digita uma descricao, da esc, e digita :wq
# git remote add origin https://github.com/brunorzm/geoboxr
# git push -u origin master

 remove.packages("geoboxr")
 remotes::install_github("brunorzm/geobox")
 library(geobox)
 geoboxr()

# para sobrescrever: git push -f origin master 

# Deploy

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()

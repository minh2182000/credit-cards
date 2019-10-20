# credit-cards
We should carefully consider the decision to apply for a credit card. If we get denied, our credits get hurt by an additional hard inquiry while no benefit is gained.

This app uses data collected on Reddit to estimate your chance of credit card approval. Beware that not many denied applications were submitted, so the results may look more optimistic than reality.

# How to set up
The following are instructions on Ubuntu 16.04 and above.
Clone this repository:
`git clone https://github.com/minhhpham/credit-cards`
`cd credit-cards`

## Install requirements
Libraries:
`sudo apt-get update`
`sudo apt-get install libcurl4-openssl-dev libssl-dev`

R:
`sudo apt-get install r-base-core`
`sudo Rscript requirements.R`

Shiny:
`sudo apt-get install gdebi-core`
`wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.12.933-amd64.deb`
`sudo gdebi shiny-server-1.5.12.933-amd64.deb`

Nginx:
`sudo apt-get install nginx`
`sudo cp ./shiny-nginx.conf /etc/nginx/conf.d/`

## Launching server
Launching shiny:
`sudo shiny-server shiny-server.conf > log.txt 2>&1 &`

Launching Nginx:
`sudo systemctl daemon-reload && sudo systemctl restart nginx`

library(CosmosToR)
vc <- vc_connection('https://cosmos11.osdinfra.net/cosmos/asimov.partner.osg/')

streamPath <- "/shares/asimov.prod.data/PublicPartner/Processed/WXaaS/MAD_DAD/Daily/2018/05/01/Win10/osEngagement_2018-05-01.ss"

data1 <- ss_all(vc, streamPath)


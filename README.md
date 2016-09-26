# Device-Atlas
To create device mapping table

Step to have the mobile atlas
1. grep the urls of all the models available on gsmarena.com using grep_url_gsmarena
2. create the table of model information using create_table.R
3. extract the technical names for each model using derive_techname.R for all brands except samsung. For samsung, use derive_techname_samsung.R

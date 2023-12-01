# ABAP_Text_Translation
SAP ABAP code to Download and Ypload translation in Mass

This Git Repository contain 2 Programs to Help ABAPer's to<br /> 

**Download Program name YPROGRAM_TEXT_DOWNLOAD**<br />
Program to download-> SAP Module pool(Screen)/Module Pool Title/ Executable program Title/Selection screenText/Text elements<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/172851b9-75c3-4b29-b2c0-4a11b308ff3c)<br />
**Module** Can only be One<br />
**Language** Soure Text Language<br />
**Program Name** Program name whose text want's to download<br />

![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/2f39dc2d-68c1-4a98-9042-1a02a070a876)<br />

**Module** The Module if you want to segrigate the List based on the Module (can only have one Module at a time)<br />
**Program Name** Selected/Entered Program name<br />
**Type** Type of Text(Report Title/Text Element/Screen-Number/Module Pool Title)<br />
**Text length** Lenght of the Text to target Translation<br />
**Key** In case of Module Pool it can be Module Pool title/Screen Variable name else it will be  Program name or text elements's number<br /> 
**Text** Actual Text of the Key.<br />

Translation Text for Message class can be downloaded form table *T100*
Translation Text for Data Types in table can be downloaded form table *DD03M*

**Upload Program name YPROGRAM_TEXT_UPLOAD**<br />
Program to upload -> SAP Module pool(Screen)/Module Pool Title/ Executable program Title/Selection screen text/Text elements/Message class/Data type Translation<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/8375fb5b-8ca0-4a08-bb39-768f992c1c82)<br />
**Radio Button** Can only perform/trigger one task at the time<br />
**Executable/ Module Pool Prg**<br />
**Message Class**<br />
**Data Type**<br />
**File name** Excel file for from the System<br />
**Source Language** Source language<br />
**Target Language**<br />
**Program/MessageClass/DataType** The proram can only perform/execute 1 tast/Program at one moment(as the run time of the FM used is too much)<br />
**CheckBox**<br />
**Save Translation** To save the text if the excel data is correct<br />
<br />
<br />
**--NOTE--**<br />
In the development of the upload program **FM ALSM_EXCEL_TO_INTERNAL_TABLE** was onverted to custom FM  as the the limit of the FM.<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/a2294e05-9052-4abf-a5e2-1675d5b7e1ff)<br />
<br />
and Structue **ALSMEX_TABLINE** was copied to create custom structure whick was used to trigger the custom FM.
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/42411523-b124-4493-a363-36d717703ca1)<br />
Extra code changes required to use Excel to Internal table FM(Copy of Function group and Function module and change in Top of Function group )<br />
<br />
Excel Upload Format for module Pool and executable program.<br />
Except **Column G** rest of the excel structure can be downloaded form Download program.<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/e578a2de-f33b-4f22-b80f-1f022f210d04)<br />
<br />
Excel Upload Format for Message Class.<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/ccd9e92b-202c-4b90-9b03-33ec40b0d5d7)<br />
<br />
Excel Upload Format for DDIC Data type.<br />
![image](https://github.com/coldblood02/ABAP_Text_Translation/assets/25544031/7f666a66-fb71-4655-924a-40fa5ffa1e43)<br />

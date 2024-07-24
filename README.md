# ABAP_TextTranslation
SAP ABAP code to Download and Ypload translation in Mass

This Git Repository contain 2 Programs to Help ABAPer's to download and upload the SAP object translation<br /> 

**Download Program name YPROGRAM_TEXT_DOWNLOAD/ T-code YTEXT_DOWN**<br />
Program to download-> SAP Module pool(Screen)/Module Pool Title/ Executable program Title/Selection screen Text/Text elements/ Class Text/Table data types/Domains<br />
![image](https://github.com/user-attachments/assets/e18e067a-d0c3-48d0-bd47-29ac0c9fd407)

<br />
**Module** Can only be One<br />
**Language** Soure Text Language<br />
**Program Name** Program name whose text want's to download<br />

![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/14d02341-f3a8-4cde-b6af-9190185d002a)<br />

**Module** The Module if you want to segrigate the List based on the Module (can only have one Module at a time)<br />
**Program Name** Selected/Entered Program name<br />
**Type** Type of Text(Report Title/Text Element/Screen-Number/Module Pool Title)<br />
**Text length** Lenght of the Text to target Translation<br />
**Key** In case of Module Pool it can be Module Pool title/Screen Variable name else it will be  Program name or text elements's number<br /> 
**Text** Actual Text of the Key.<br />

Translation Text for Message class can be downloaded form table *T100*
Translation Text for Data Types in table can be downloaded form table *DD03M*

**Upload Program name YPROGRAM_TEXT_UPLOAD/ T-code YTEXT_UP**<br />
Program to upload -> SAP Module pool(Screen)/Module Pool Title/ Executable program Title/Selection screen text/Text elements/Message class/Data type Translation<br />
![image](https://github.com/user-attachments/assets/42c75f20-e373-4a9c-98d0-a8eb4e514572)
<br />
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
![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/3b674869-3814-44fd-a8aa-75797b149aa8)<br />
<br />
and Structue **ALSMEX_TABLINE** was copied to create custom structure whick was used to trigger the custom FM.
![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/15196037-cd7d-433e-9765-0a4ddc6fbbf2)<br />
Extra code changes required to use Excel to Internal table FM(Copy of Function group and Function module and change in Top of Function group )<br />
<br />
Excel Upload Format for module Pool and executable program.<br />
Except **Column G** rest of the excel structure can be downloaded form Download program.<br />
![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/468afe74-489f-4cae-8494-e20207121000)<br />
<br />
Excel Upload Format for Message Class.<br />
![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/29713e9f-ec4c-4b40-b9c2-16c6f344c56f)<br />
<br />
Excel Upload Format for DDIC Data type.<br />
![image](https://github.com/coldb02/ABAP_TextTranslation/assets/25544031/88f851cb-c574-4a52-87d2-1d645e098f0e)<br />

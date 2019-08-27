
for (UID in UserList){
    UserTbl_Email <- NF_Target_User %>% filter(`User #` %in% UID)
    User1_Email<- c(UserTbl_Email$Email_Address)
    User1_FullNM <- c(UserTbl_Email$Full_Name)
    User1_SecID <- c(UserTbl_Email$Name)
    OutApp <- COMCreate("Outlook.Application")
    #create an email
    outMail = OutApp$CreateItem(0)
    #configure  email parameter
    outMail[["To"]] = (User1_Email)
    outMail[["CC"]] = ("Jeremy.Ralya@jackson.com")
    outMail[["subject"]] = paste(" NF Daily Direction: Secret Identity Assigned")


    outMail[["body"]] = paste("Good Morning :", User1_FullNM, ",


							  Your secret identity for the day is:", User1_SecID, ". This identity will allow you to remain anonymous while you monitor your intra-day stats on the Team Leader-board that will be included with Daily Direction updates sent throughout the afternoon.

                              Do NOT share your secret identity with your peers. Your stats are for your eyes only. If you have any questions, please reach out to your manager.

                              Thank You for all you do,

                              Director, Nick Fury

                              Note: This is an automated message.")



    outMail$Send()

}
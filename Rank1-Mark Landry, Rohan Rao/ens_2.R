test_contacts <- fread("Contacts2017.csv")

s1 <- fread("Contacts1.csv")
s2 <- fread("Contacts2.csv")

setorder(s1, ID)
setorder(s2, ID)

s2$Contacts[s2$CONTACT.TYPE == "Internal Management"] <- s1$Contacts[s1$CONTACT.TYPE == "Internal Management"]

s <- copy(s1)
s$Contacts <- 0.7 * s1$Contacts + 0.3 * s2$Contacts

fwrite(s, "Contacts.csv")

s1 <- fread("Resolution1.csv")
s2 <- fread("Resolution2.csv")

setorder(s1, ID)
setorder(s2, ID)

s <- copy(s1)
s$Resolution <- 0.7 * s1$Resolution + 0.3 * s2$Resolution

fwrite(s, "./submission/Resolution.csv")

# Run the application 
source_url("https://raw.githubusercontent.com/sshankha/shiny-author-affiliations/main/authors_list.R")
shinyApp(ui = ui, server = server)

#have an excel file with the following columns 
name
sequence - this will be used to sequence the authors' list
E-mail
affiliation1 -primary affiliation 
affiliation2 -secondary affiliation 
For authors' contribution use the following with "x" for yes.  
Sample processing 
quality control and administration 
Data generation	
Data freeze	
Data interpretation	
Data analysis	
Figures	
Manuscript writing	
Supervision 	
Funding acquisition

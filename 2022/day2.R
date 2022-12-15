input <- read.csv("input.txt", sep=" ", header=F)
input$V2
(sum(input$V2=="X")*1+sum(input$V2=="Y")*2+sum(input$V2=="Z")*3)+
sum(case_when((input$V2=="X" & input$V1=="C") |
            (input$V2=="Y" & input$V1=="A") |
            (input$V2=="Z" & input$V1=="B") ~ 6,
          (input$V2=="X" & input$V1=="A") |
            (input$V2=="Y" & input$V1=="B") |
            (input$V2=="Z" & input$V1=="C") ~ 3,
          TRUE ~ 0))

#############
(sum(input$V2=="X")*0+sum(input$V2=="Y")*3+sum(input$V2=="Z")*6)+
  sum(case_when((input$V1=="A" & input$V2=="Y") |
                  (input$V1=="B" & input$V2=="X") |
                  (input$V1=="C" & input$V2=="Z") ~ 1,
                (input$V1=="B" & input$V2=="Y") |
                  (input$V1=="C" & input$V2=="X") |
                  (input$V1=="A" & input$V2=="Z") ~ 2,
                (input$V1=="C" & input$V2=="Y") |
                  (input$V1=="A" & input$V2=="X") |
                  (input$V1=="B" & input$V2=="Z") ~ 3))

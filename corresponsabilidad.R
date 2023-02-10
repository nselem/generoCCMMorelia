#### Cargamos paquetes ####
library("devtools")
library("dplyr")
library("vroom")
library("tidyr")
library("ggplot2")
library("lubridate")

#### Cargamos datos ####
corresponsabilidad <- vroom ( file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRWYRnCkaMShbsQazisRqZtEM5s112DrC6Y6CX_X2WYFemIwy5HrsnBTc3OocwlW9MsrFeejRjTUQRi/pub?output=csv", show_col_types = F) #uso vroom porque me deja leer mas fácil los files csv

colnames(corresponsabilidad)<- c("fecha", "dependientes", "genero", "edad", "menos", "justo", "mas","hace_mas")

df_genero<-corresponsabilidad%>%select(c(genero,hace_mas))
mujeres=length(df_genero$genero[df_genero$genero == "Mujer"])
hombres=length(df_genero$genero[df_genero$genero == "Hombre"])
otros=length(df_genero$genero[df_genero$genero == "Otro"])

df_edad<-corresponsabilidad%>%select(c(edad,hace_mas))
df_dependientes<-corresponsabilidad%>%select(c(genero,dependientes))


tabla1<-table(df_genero)
tabla2<-table(df_edad)

plot(tabla1, col = c("red", "blue","yellow"), main = "genero vs. genero que hace más mantenimiento")

plot(tabla2, col = c("red", "blue","yellow","orange"), main = "edad vs. genero que hace más mantenimiento")
levels(as.factor(corresponsabilidad$dependientes))
# Factor levels
# 1 No tengo personas dependientes,
# 2 No tengo personas dependientes, Realizo trabajo de mantenimiento del hogar
# 3 Tengo personas dependientes,
# 4 Tengo personas dependientes, No tengo personas dependientes, Realizo trabajo de mantenimiento del hogar
# 5 Tengo personas dependientes, Realizo trabajo de mantenimiento del hogar
#v1=c(1,2)
depend=c(3,4,5)
si_dependientes1<-as.integer(as.integer(as.factor(corresponsabilidad$dependientes)) %in% (depend))
LabHog=c(2,4,5)
si_hogar1<-as.integer(as.integer(as.factor(corresponsabilidad$dependientes))%in% LabHog )
#as.integer(as.factor(corresponsabilidad$dependientes))
df_dependientes$hogar<-si_hogar1
df_dependientes$dependientes<-si_dependientes1


df_rel_muj<-df_dependientes %>%
  filter(genero=="Mujer") %>%
  group_by(dependientes) %>% summarize(total=n())%>% mutate(porcentaje=total/mujeres)%>% mutate(genero="mujeres")

df_rel_hombres<-df_dependientes %>%
  filter(genero=="Hombre") %>%
  group_by(dependientes) %>% summarize(total=n())%>% mutate(porcentaje=total/hombres)%>% mutate(genero="hombres")

df_rel_otro<-df_dependientes %>%
  filter(genero=="Otro") %>%
  group_by(dependientes) %>% summarize(total=n())%>% mutate(porcentaje=total/otros)%>% mutate(genero="otros")


de_rel<-rbind(df_rel_muj,df_rel_hombres,df_rel_otro )

de_rel

##########333
#ggplot(data = df_dependientes)+

ggplot(data= de_rel, aes(x=genero, y=porcentaje, fill=as.factor(dependientes)))+
  geom_bar(aes(), stat="identity", position="stack") +
  coord_flip()  +
  xlab("Género") +
  ylab("Porcentaje de dependientes") +
  ggtitle("Porcentaje de dependientes por género") +
    labs(fill = "Géneros") +
    coord_flip()

##########333
df_rel_muj<-df_dependientes %>%
  filter(genero=="Mujer") %>%
  group_by(hogar) %>% summarize(total=n())%>% mutate(porcentaje=total/mujeres)%>% mutate(genero="mujeres")

df_rel_hombres<-df_dependientes %>%
  filter(genero=="Hombre") %>%
  group_by(hogar) %>% summarize(total=n())%>% mutate(porcentaje=total/hombres)%>% mutate(genero="hombres")

df_rel_otro<-df_dependientes %>%
  filter(genero=="Otro") %>%
  group_by(hogar) %>% summarize(total=n())%>% mutate(porcentaje=total/otros)%>% mutate(genero="otros")


de_rel<-rbind(df_rel_muj,df_rel_hombres,df_rel_otro )

de_rel

##########333
#ggplot(data = df_dependientes)+

ggplot(data= de_rel, aes(x=genero, y=porcentaje, fill=as.factor(hogar)))+
  geom_bar(aes(), stat="identity", position="stack") +
  coord_flip()  +
  xlab("Género") +
  ylab("Porcentaje de labores del hogar") +
  ggtitle("Porcentaje de labores del hogar por género") +
  labs(fill = "Géneros") +
  coord_flip()

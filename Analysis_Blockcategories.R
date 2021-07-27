# Load the package required to read JSON files.
library("rjson")
library(ggplot2)
library("writexl")


#setting working directory
setwd("~/Documents/ScratchData_test/all_projects/")
#when ignoring remixed projects: no_remix

blocks_allprojects<-list()
length_allprojects<-list()
project_withprocedure<-list()
project_withlist<-list()
project_withvariable<-list()
weirdshadows<-list()

variable <-c("data_setvariableto","data_changevariableby","data_hidevariable","data_showvariable")
slist <-c ("data_listcontents", "data_addtolist", "data_deleteoflist","data_deletealloflist", "data_insertatlist", "data_replaceitemoflist", "data_itemoflist", "data_itemnumoflist", "data_lengthoflist", "data_listcontainsitem", "data_hidelist", "data_showlist")


for (file_i in list.files()){
  
  print(paste0("file number:",file_i))
  
  # Give the input file name to the function.
  result <- fromJSON(file = file_i)
  
  #looks for targets
  find_targets <- unlist(lapply(result,function(x) any(names(x)%in% "targets")))
  result=result[[which(find_targets)]]
  
  #getting rid of Shadow blocks
  for (i in 1:length(result$targets)){
    shadows <- sapply(result$targets[[i]]$blocks, function(x){
      x$shadow
    })
    if(length(shadows) >0) result$targets[[i]]$blocks[shadows] <- NULL
  }
  
  
  #Info about the blocks
  blockinfo <- function(dat, target,name=TRUE,length=TRUE,types=TRUE){
    out<-list()
    if(name) out$name = dat$targets[[target]]$name
    if(length) out$length= length(dat$targets[[target]]$blocks)
    if(types) {
      out$blocks <- sapply(dat$targets[[target]]$blocks, function(y){
        y$opcode
      })
      names(out$blocks) <- NULL
    }
    return(out)
  }
  
  # for all targets
  all_blocks <- c()
  for (i in 1:length(result$targets)){
    all_blocks <- c(all_blocks,unlist(blockinfo(result,name=FALSE,length=FALSE, target =i)$blocks))
    #print(paste0("traget:",i))
    #print(all_blocks)
  } 
  
  #checking if lists, variables or procedures
  if (length(all_blocks[grep("^procedures",all_blocks)]) > 0){
    project_withprocedure[[file_i]]<-all_blocks[grep("^procedures",all_blocks)] 
  }
  
  if (any(unlist(lapply(all_blocks,function(x) x %in% variable)))){
    project_withvariable[[file_i]]<- unlist(lapply(all_blocks,function(x){
      return(x[x%in% variable])
    }))
  }
  
  
  if (any(unlist(lapply(all_blocks,function(x) x %in% slist)))){
    project_withlist[[file_i]]<-unlist(lapply(all_blocks,function(x){
      return(x[x%in% slist])
    })) 
  }

  #categories
  for (i in 1:length(all_blocks)){
    all_blocks[grep("^motion",all_blocks)] = "motion"
    all_blocks[grep("^control",all_blocks)] = "control"
    all_blocks[grep("^event",all_blocks)] = "event"
    all_blocks[grep("^looks",all_blocks)] = "looks"
    all_blocks[grep("^operator",all_blocks)] = "operators"
    all_blocks[grep("^sensing",all_blocks)] = "sensing"
    all_blocks[grep("^sound",all_blocks)] = "sound"
    all_blocks[grep("^procedures",all_blocks)] = "data"
    all_blocks[grep("^data",all_blocks)] = "data" #this contains variables and lists
  }
  
  
  #saving informations of all projects
  blocks_allprojects[[file_i]] = all_blocks
  length_allprojects[[file_i]] = length(all_blocks)
  
  #making a table
  blocksplot <- table(all_blocks)
  
  #number of blocks
  print(paste0("total number of blocks: ", length(all_blocks)))
  
  
}



#plotting the blocks of all projects in a bar table
unlist_allprojects <-unlist(blocks_allprojects)

#setting blocks that are not in the categories in "others", doing it only now since I want to keep track of it before
blockstypes<-c("motion", "control","event","looks","operators","sensing","sound","data","procedures")

other_blocks<-unlist_allprojects[!unlist_allprojects %in% blockstypes]
print(paste0("other blocks:",other_blocks))
unlist_allprojects[!unlist_allprojects %in% blockstypes]="others"

blocksplot <- table(unlist_allprojects)

blocksplot_pc <- as.data.frame(blocksplot/sum(blocksplot)*100)

#plotting as a barplot with different colors (needs to be a dataframe)
print(ggplot(blocksplot_pc, aes(x=unlist_allprojects, y = Freq, fill = unlist_allprojects)) + 
        ggtitle("Blocktypes in Projects")+ 
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        scale_x_discrete(limits=c("motion","looks","sound","event","control","sensing", "operators", "data", "others")) + 
        labs(x="categories", y="percentages") + 
        scale_fill_manual(breaks=c("motion","looks","sound","event","control","sensing", "operators", "data", "others"),
                          values=c(motion = "#3a3df0",looks="#923af0",sound= "#de3af0", event="#f0c93a",control="#f0a73a",sensing="#77bdf2",operators="#4dab68", data = "#ff872e", others = "#000000"))
)

#plotting comparison with literature

data_aiv<-read.csv(file="avi.csv", sep=";")
aiv_perc<- data_aiv/sum(data_aiv)*100
aiv_perc=data.frame(types = colnames(aiv_perc),Freq=unlist(aiv_perc))


blocksplot_pc$Source="Scratch Challenge"
aiv_perc$Source="Aivaloglou and Hermans [1]"
colnames(blocksplot_pc)[1] = "types"

all_data <-rbind(blocksplot_pc, aiv_perc)
all_data$Source <- factor(all_data$Source,levels=c("Scratch Challenge","Aivaloglou and Hermans [1]"))

print(ggplot(all_data, aes(x=types, y =Freq, fill = Source)) + 
        #ggtitle("Types of Blocks")+
        geom_bar(position="dodge", stat = "identity") + 
        theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 20)) +
      scale_x_discrete(limits=c("control", "looks", "event","motion","operators","data","sensing","sound", "others")) + 
      labs(x="Block categories", y="Percentages") #+ 
      #scale_fill_manual(breaks=c("motion","looks","sound","event","control","sensing", "operators","data", "others"),
      #values=c(motion = "#3a3df0", "#3a3df0",looks="#923af0","#3a3df0",sound= "#de3af0", event="#f0c93a",control="#f0a73a",sensing="#77bdf2",operators="#4dab68", data = "#ff872e", others = "#000000"))
)

#plotting number of blocks per project
length_allprojects <-unlist(length_allprojects)

x = length_allprojects


print(ggplot(data.frame(x), aes(x)) +               # Histogram with log10 axis
  geom_histogram(bins = 8, color="black", fill="black", position=position_dodge2(width=1, padding=0.5, preserve = "single"))+
    theme( text = element_text(size = 20)) +
  #scale_x_binned(breaks = c(2, 4, 8, 16,32, 64,128,256,512,1024,2048,4096))+
  scale_x_log10(breaks = c(2, 4, 8, 16,32, 64,128,256,512,1024,2048,4096))+
    #ggtitle("Number of Blocks per Projekt")+ 
  #scale_x_continuous(breaks = c(2, 4, 6))+
  labs(x="Number of blocks per project", y="Frequencies")  
)


print(ggplot(data.frame(x), aes(x)) +               # Histogram with log2 axis
        geom_histogram(stat="count", position = position_dodge2(width=1,padding=20))+
        #scale_x_binned(breaks = c(2, 4, 8, 16,32, 64,128,256,512,1024,2048,4096))+
        scale_x_binned(breaks = c(2, 4, 8, 16,32, 64,128,256,512,1024,2048,4096), trans="log2")+
        ggtitle("Number of Blocks per Projekt")+ 
        #scale_x_continuous(breaks = c(2, 4, 6))+
        labs(x="number of blocks per project (log scale)", y="frequencies")  
)

print(ggplot(data.frame(x), aes(x)) +
  geom_bar(position=position_dodge2(width=2, padding=20)) +
  #scale_x_binned()+
  scale_x_log10() +
  stat_bin(breaks =c(2, 4, 6, 8, 16,32, 64,128,256,512,1024,2048,4096))
  
)

#plotting comparison with literature

sc_pp<-read.csv(file="sc_pp.csv", sep=";")
sc_pp=data.frame(types = colnames(sc_pp),Freq=unlist(sc_pp))

aiv_pp<-read.csv(file="aiv_pp.csv", sep=";")
aiv_pp=data.frame(types = colnames(aiv_pp),Freq=unlist(aiv_pp))


sc_pp$Source="Scratch Challenge"
aiv_pp$Source="Aivaloglou and Hermans [1]"

all_data_pp <-rbind(aiv_pp, sc_pp)
all_data_pp$Source <- factor(all_data_pp$Source,levels=c("Scratch Challenge","Aivaloglou and Hermans [1]"))


print(ggplot(all_data_pp, aes(x=types, y =Freq, fill = Source)) + 
        #ggtitle("Types of Blocks")+
        geom_bar(position="dodge", stat = "identity") + 
        theme(legend.position = "bottom", legend.title = element_blank(), 
              text = element_text(size = 20)) +
        scale_x_discrete(limits=c("Variables", "Lists","Procedures")) + 
        labs(x="Block categories", y="Percentages of Projects using the category") #+ 
      #scale_fill_manual(breaks=c ("Scratch Challenge", "Aivaloglou and Hermans [1]"), values=c("Scratch Challenge" = "red", "Aivaloglou and Hermans [1]" = "blue")
                          )

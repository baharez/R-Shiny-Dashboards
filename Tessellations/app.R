
# https://zahirodini.shinyapps.io/tessellations/

library(shiny)
library(shinydashboard)
library(NLRoot)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(dplyr)

library(readxl)
tab_data <- read_excel("tab-data.xlsx")
tab_df = data.frame(tab_data)
tab_df = tab_df[order(tab_df$rhs),]
tab_df_1 = tab_df[,c("rhs","support","X1.ME","Political.Party","Gender","Age.Category")]
colnames(tab_df_1) = c("Question", "Support","CI.Inverse","Political.Party","Gender","Age.Category")
questions=c('Q11_A' , 'Q11_B' , 'Q11_C' , 'Q11_D' , 'Q11_E' , 'Q11_F' , 'Q11_G' , 'Q11_H' , 'Q11_I' , 'Q11_J' , 'Q11_K' , 'Q11_L' , 'Q11_M' , 'Q11_N' , 'Q11_O' , 'Q11_P' , 'Q11_Q' , 'Q11_R' )



# Define UI for application that draws
ui = dashboardPage(
  
  # Application title
  dashboardHeader(title = "Tessellations"),
  
  # Sidebar with a slider input for number of bins
    dashboardSidebar(
      suppressWarnings(selectizeInput("reference",
                                      "Select a reference question",
                                      choices = questions ,
                                      selected="Q11_A")),
      suppressWarnings(selectizeInput("compared",
                                      "Select a question to compare",
                                      choices = questions ,
                                      selected="Q11_B")),
      sliderInput("depth",
                  "Depth",
                  min = 1,
                  max = 6,
                  value = 1),
      radioButtons("gender",
                   "Gender",
                   choices = c('Female' = '2',
                               'Male'= '1'),
                   selected='1'),
      radioButtons("age",
                   "Age",
                   choices = c('Less than 50' = '1',
                               'More than 50 '= '2'),
                   selected='2'),
      radioButtons("party",
                   "Political Party",
                   choices = c('Republican' = '2',
                               'Democrat'= '1'),
                   selected='1')
      
      
    ),
    dashboardBody(
      fluidRow(
      box(h5("You've just seen a customized selection based on age group, political party, and gender. Dive deeper into your chosen demographic group and interact with the visualizations to watch as the tessellation dynamically illustrates your selected group's patterns. Enjoy discovering new perspectives!"),width = 12)
      ),
      fluidRow(
           box(title = "Perefect Tessellation",
                status = "primary",
                solidHeader = TRUE,
                width=6,
                plotOutput("perfectPlot", height = 300)),

      
        box(title = "Imperefect Tessellation",
            status = "primary",
            solidHeader = TRUE,
            width=6,
            plotOutput("imperfectPlot", height = 300))
        
      ),
      fluidRow(
        column(6,
                fluidRow(h4("Details:")),
                fluidRow(tableOutput("perfect_table"))),
        column(6,
                 fluidRow(h4("Details:")),
                 fluidRow(tableOutput("imperfect_table"))
        )
        
      )
      ),
    
    # Show a plot of the generated distribution
    #mainPanel(
     # plotOutput("distPlot")
    #)
  )


# Define server logic required to draw
server <- function(input, output) {
  
    #--library(NLRoot), library(tidyverse), library(ggthemes), library(gridextra)--
    
    #d= subset(tab_df_1,Question == input$compared & Political.Party== input$party & Gender == input$gender & Age.Category == input$age)

    
    ## Non-linear root finding ###
    nl.root<-function(f, a, b, num = 10, eps = 1e-05)
    {
      h = abs(b - a)/num
      i = 0
      j = 0
      a1 = b1 = 0
      while (i <= num) {
        a1 = a + i * h
        b1 = a1 + h
        if (f(a1) == 0) {
          print(a1)
          print(f(a1))
        }
        else if (f(b1) == 0) {
          print(b1)
          print(f(b1))
        }
        else if (f(a1) * f(b1) < 0) {
          repeat {
            if (abs(b1 - a1) < eps)
              break
            x <- (a1 + b1)/2
            if (f(a1) * f(x) < 0)
              b1 <- x
            else a1 <- x
          }
          #   print(j + 1)
          j = j + 1
          return((a1 + b1)/2)
          #  print(f((a1 + b1)/2))
        }
        i = i + 1
      }
      
    }
    
    
    #comp_data <- tab_df_1 %>% filter(Political.Party == input$party, Gender == input$gender, Age.Category == input$age, Question == questions[compared])
    
    tesselation.comparison <- function(reference,compared,gender,depth,colpal,filtered_data) {
      
     
      
      comp_data <- filtered_data %>% filter(Question == compared)
      ref_data <- filtered_data %>% filter(Question == reference)
      
      func.ref <- function(p) {
        ((3 - 4 * p^2) / (2 * p * (1 - p^2)^0.5)) - (ref_data$Support / (1 / ref_data$CI.Inverse))
      }
      func.comp <- function(p) {
        ((3 - 4 * p^2) / (2 * p * (1 - p^2)^0.5)) - (comp_data$Support / (1 / comp_data$CI.Inverse))
      }
      a.ref=nl.root(func.ref, 0.01, 0.99)
      angle.ref=asin(a.ref)
      angle.deg.ref=angle.ref*(180/pi)
      angle.deg.ref
      
      a.comp=nl.root(func.comp, 0.01, 0.99)
      angle.comp=asin(a.comp)
      angle.deg.comp=angle.comp*(180/pi)
      angle.deg.comp
      

      ################################################################################################
      CreateKite <- function() {
        scale.ref=angle.deg.ref
        c <- 5000
        A <- (36/scale.ref)*angle.deg.ref
        B <- 2*A
        C <- 180 - (A + B)
        b <- sin(B*pi/180)*(c/sin(C*pi/180))
        x <- c(0, c, b*cos(A*pi/180))
        y <- c(0, 0, b*sin(A*pi/180))
        st <- c(3, 1, 2)
        xend <- lead(x, default = 0)
        yend <- lead(y, default = 0)
        tibble(x = x, y = y, xend = xend, yend = yend, st = st) %>%
          bind_rows(tibble(x = x, y = -y, xend = xend, yend = -yend, st = st))
      }
      
      
      RotatePolygon <- function(df, angle) {
        df %>% transmute(x2    = cos(angle*pi/180)*x - sin(angle*pi/180)*y,
                         y2    = sin(angle*pi/180)*x + cos(angle*pi/180)*y,
                         xend2 = cos(angle*pi/180)*xend - sin(angle*pi/180)*yend,
                         yend2 = sin(angle*pi/180)*xend + cos(angle*pi/180)*yend,
                         st = st) %>%
          rename_all(str_replace_all, "2", "")
      }
      
      
      
      DivideTriangle123 <- function(df) {
        # Add longitude of sides to df
        df %>%
          mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
        
        # Longitude of the new side 3
        df %>%
          filter(st == 1) %>%
          select(lng) %>% as.numeric -> d1
        
        # Gradient of the new side 3
        df %>%
          filter(st == 3) %>%
          transmute(gx = xend - x,
                    gy = yend - y) %>% as.numeric() -> g1
        
        # Inner point of side 3 to divide the triangle
        df %>% filter(st == 3) %>% select(x, y) %>%
          as.numeric() + d1 * g1/sqrt(sum(g1^2)) -> p1
        
        # # Longitude
        # df %>%
        #   filter(st == 1) %>%
        #   select(lng) %>% as.numeric -> d2
        #
        
        # Gradient to find the second point p2
        df %>%
          filter(st == 2) %>%
          transmute(gx = xend - x,
                    gy = yend - y) %>% as.numeric() -> g2
        
        # Point p2
        df %>%
          filter(st == 2) %>%
          select(x, y) %>%
          as.numeric() + d1 * g2/sqrt(sum(g2^2)) -> p2
        
        # df1: subtriangle (type 123)
        c(p1,
          df %>% filter(st == 3) %>% select(xend, yend) %>% as.numeric()) %>%
          rbind(c(df %>% filter(st == 1) %>% select(x, y) %>% as.numeric(),
                  df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(),
                  p1)) -> df1
        
        colnames(df1) <- c("x", "y", "xend", "yend")
        
        # st: labels of new sides
        as_tibble(df1) %>% mutate(st = 1:3) -> df1
        
        # df2: subtriangle (type 123)
        c(p1,
          p2) %>%
          rbind(c(p2,
                  df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(),
                  p1)) -> df2
        
        colnames(df2) <- c("x", "y", "xend", "yend")
        as_tibble(df2) %>% mutate(st = 1:3) -> df2
        
        # df3 subtriangle (type 124)
        c(p1,
          p2) %>%
          rbind(c(p2,
                  df %>% filter(st == 3) %>% select(x, y) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 3) %>% select(x, y) %>% as.numeric(),
                  p1)) -> df3
        
        colnames(df3) <- c("x", "y", "xend", "yend")
        as_tibble(df3) %>% mutate(st = c(1, 4, 2)) -> df3
        
        # Binding three triangles
        bind_rows(df1, df2, df3) %>% round(0)
        
      }
      
      
      DivideTriangle124 <- function(df) {
        # Add longitude  
        df %>%
          mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
        
        df %>%
          filter(st == 1) %>%
          select(lng) %>% as.numeric -> d1
        
        # This is the longitude of the inner side to subdivide triangle
        x <- d1*sin(36*pi/180)/sin(108*pi/180)
        
        # Gradient to calculate point
        df %>%
          filter(st == 2) %>%
          transmute(gx = x - xend,
                    gy = y - yend) %>% as.numeric() -> g2
        
        # Inner point side 2
        df %>%
          filter(st == 2) %>%
          select(xend, yend) %>% as.numeric() + x * g2/sqrt(sum(g2^2)) -> p1
        
        # df1 subtriangle (type 123)
        c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
          p1) %>%
          rbind(c(p1,
                  df %>% filter(st == 2) %>% select(x, y) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 2) %>% select(x, y) %>% as.numeric(),
                  df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df1
        
        colnames(df1) <- c("x", "y", "xend", "yend")
        as_tibble(df1) %>% mutate(st = 1:3) -> df1
        
        # df2 subtriangle (type 124)
        c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
          p1) %>%
          rbind(c(p1,
                  df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric(),
                  df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df2
        
        colnames(df2) <- c("x", "y", "xend", "yend")
        as_tibble(df2) %>% mutate(st = c(1,4,2)) -> df2
        # Binding two triangles
        bind_rows(df1, df2) %>% round(0)
        
      }
      
      
      Divide <- function(df)
      {
        if(df %>% select(st) %>% sum() %>% as.numeric == 6)
          DivideTriangle123(df)
        else
          DivideTriangle124(df)
      }
      
      
      Divide_df <- function(df){
        df %>%
          mutate(poly_id=rep(1:((nrow(.))/3), each=3)) %>%
          group_by(poly_id) %>%
          group_split() %>% lapply(Divide) %>%  
          bind_rows()
      }
      
      
      Arrange_df <- function(df){
        df %>% round(0) -> df
        
        # Unique points which define segments
        bind_rows(df %>% select(xx = x, yy = y),
                  df %>% select(xx = xend, yy = yend)) %>% unique -> points
        
        # Distance betweem points
        dist_mat <- dist(points, method = 'euclidean')
        hclust_avg <- hclust(dist_mat, method = 'average')
        
        # Group points
        points %>% mutate(group = cutree(hclust_avg, h = 8)) -> points
        
        # Centroid of each cluster
        points %>% group_by(group) %>% summarize(xnew = mean(xx),
                                                 ynew = mean(yy)) -> mean_points
        
        # Substitute each point by its centroid
        points %>% inner_join(mean_points, by = "group") -> points
        
        # Recreate df
        df %>%
          inner_join(points, by = c("x"="xx", "y"="yy")) %>%
          mutate(x = xnew,
                 y = ynew) %>%
          select(-xnew, -ynew) %>%
          left_join(points, by = c("xend"="xx", "yend"="yy")) %>%  
          mutate(xend = xnew,
                 yend = ynew) %>%
          select(-xnew, -ynew) -> df
        
        
        df %>% mutate(poly_id = rep(1:((nrow(.))/3), each=3) %>% as.character) -> df
        
        
        df %>% filter(st %in% c(3,4)) -> df_sub
        
        
        union(df_sub %>%
                inner_join(df_sub, by = c("x","y", "xend", "yend"), suffix = c(".1", ".2")),
              df_sub %>%
                inner_join(df_sub, by = c("x"="xend", "y"="yend","xend"="x", "yend"="y"), suffix = c(".1", ".2"))) %>%
          filter(poly_id.1 != poly_id.2) %>%
          select(poly_id.1, poly_id.2) %>%
          mutate(poly_id_new=apply(., 1, FUN = function (r) sort(r) %>% paste(collapse="_"))) %>%
          gather(condition, poly_id, poly_id.1:poly_id.2) %>%
          select(-condition) %>%
          distinct -> df_new
        
        # New df with definitive polygons
        df %>%
          inner_join(df_new, by = "poly_id") %>%
          mutate(remove = st %in% c(3, 4) & !is.na(poly_id_new),
                 poly_id = ifelse(is.na(poly_id_new), poly_id, poly_id_new)) %>%
          filter(remove == FALSE) %>%
          select(-poly_id_new, -remove) %>% unique -> df
        
        
        return(df)
        
      }
      
      
      Create_Polygon <- function(df)
      {
        bind_rows(inner_join(df %>% slice(1),
                             df %>% slice(-1),
                             by = c("xend" = "x", "yend" = "y")) %>%
                    select(x1 = x, y1 = y, x2 = xend, y2 = yend, x3 = xend.y, y3 = yend.y),
                  
                  inner_join(df %>% slice(1),
                             df %>% slice(-1),
                             by = c("xend" = "xend", "yend" = "yend")) %>%
                    select(x1=x.x, y1=y.x, x2=xend, y2=yend, x3 = x.y, y3 = y.y)) -> temp
        
        bind_rows(temp %>% select(x = x1, y = y1),
                  temp %>% select(x = x2, y = y2),
                  temp %>% select(x = x3, y = y3)) -> sub1
        
        bind_rows(df %>% select(x,y),
                  df %>% select(x=xend,y=yend)) %>% unique -> points
        
        bind_rows(sub1,
                  points %>% anti_join(sub1, by = c("x", "y")),
                  sub1 %>% slice(1))
        
      }
      
      
      niter <- depth
      
      init_kite <- CreateKite()
      
      
      seq(from = 0, by=72, length.out = 5) %>%
        lapply(function(x) RotatePolygon(init_kite, angle = x)) %>%
        bind_rows() %>%
        round(0) -> init_polygon
      
      
      
      reduce(
        .x = 1:niter,
        .f = function(old, y) {Divide_df(old)},
        .init=init_polygon) -> raw_tessellation
      
      
      segment_tessellation <- Arrange_df(raw_tessellation)
      
      
      segment_tessellation %>% group_by(poly_id) %>%
        group_split() %>% lapply(Create_Polygon) %>%  
        bind_rows(.id = "poly_id") -> poly_tessellation
      
      
      poly_tessellation %>%
        group_by(poly_id) %>%
        slice(-1) %>%
        mutate(ldx = lead(x, default = first(x)),
               ldy = lead(y, default = first(y))) %>%
        summarize(area=0.5*abs(sum(x*ldy - ldx*y))) -> areas
      
      
      poly_tessellation %>% inner_join(areas, by = "poly_id") -> poly_tessellation
      
      ref_title <- paste("Question 11", reference ,"Tesselation depth:", niter)
      
      plot.ref=ggplot(poly_tessellation) +
        geom_polygon(aes(x=x, y=y, group = poly_id, fill = area),
                     col = "gray65",  
                     lwd = 0,
                     show.legend = NA) +
        scale_fill_gradientn(colours = c(colpal[2],colpal[3] )) +
        coord_equal() +
        theme_void() +
        theme(legend.position='none')+theme(plot.background = element_rect(fill = colpal[1], colour = 'red'))+
        ggtitle(ref_title)
      
      
      
      ##############################################################################
      
      CreateKite <- function() {
        scale.ref=angle.deg.ref
        c <- 5000
        A <- (36/scale.ref)*angle.deg.comp
        B <- 2*A
        C <- 180 - (A + B)
        b <- sin(B*pi/180)*(c/sin(C*pi/180))
        x <- c(0, c, b*cos(A*pi/180))
        y <- c(0, 0, b*sin(A*pi/180))
        st <- c(3, 1, 2)
        xend <- lead(x, default = 0)
        yend <- lead(y, default = 0)
        tibble(x = x, y = y, xend = xend, yend = yend, st = st) %>%
          bind_rows(tibble(x = x, y = -y, xend = xend, yend = -yend, st = st))
      }
      
      
      RotatePolygon <- function(df, angle) {
        df %>% transmute(x2    = cos(angle*pi/180)*x - sin(angle*pi/180)*y,
                         y2    = sin(angle*pi/180)*x + cos(angle*pi/180)*y,
                         xend2 = cos(angle*pi/180)*xend - sin(angle*pi/180)*yend,
                         yend2 = sin(angle*pi/180)*xend + cos(angle*pi/180)*yend,
                         st = st) %>%
          rename_all(str_replace_all, "2", "")
      }
      
      
      
      DivideTriangle123 <- function(df) {
        # Add longitude of sides to df
        df %>%
          mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
        
        # Longitude of the new side 3
        df %>%
          filter(st == 1) %>%
          select(lng) %>% as.numeric -> d1
        
        # Gradient of the new side 3
        df %>%
          filter(st == 3) %>%
          transmute(gx = xend - x,
                    gy = yend - y) %>% as.numeric() -> g1
        
        # Inner point of side 3 to divide the triangle
        df %>% filter(st == 3) %>% select(x, y) %>%
          as.numeric() + d1 * g1/sqrt(sum(g1^2)) -> p1
        
        
        df %>%
          filter(st == 2) %>%
          transmute(gx = xend - x,
                    gy = yend - y) %>% as.numeric() -> g2
        
        # Point p2
        df %>%
          filter(st == 2) %>%
          select(x, y) %>%
          as.numeric() + d1 * g2/sqrt(sum(g2^2)) -> p2
        
        # df1: subtriangle (type 123)
        c(p1,
          df %>% filter(st == 3) %>% select(xend, yend) %>% as.numeric()) %>%
          rbind(c(df %>% filter(st == 1) %>% select(x, y) %>% as.numeric(),
                  df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(),
                  p1)) -> df1
        
        colnames(df1) <- c("x", "y", "xend", "yend")
        
        # st: labels of new sides
        as_tibble(df1) %>% mutate(st = 1:3) -> df1
        
        # df2: subtriangle (type 123)
        c(p1,
          p2) %>%
          rbind(c(p2,
                  df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(),
                  p1)) -> df2
        
        colnames(df2) <- c("x", "y", "xend", "yend")
        as_tibble(df2) %>% mutate(st = 1:3) -> df2
        
        # df3 subtriangle (type 124)
        c(p1,
          p2) %>%
          rbind(c(p2,
                  df %>% filter(st == 3) %>% select(x, y) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 3) %>% select(x, y) %>% as.numeric(),
                  p1)) -> df3
        
        colnames(df3) <- c("x", "y", "xend", "yend")
        as_tibble(df3) %>% mutate(st = c(1, 4, 2)) -> df3
        
        # Binding three triangles
        bind_rows(df1, df2, df3) %>% round(0)
        
      }
      
      
      DivideTriangle124 <- function(df) {
        # Add longitude  
        df %>%
          mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
        
        df %>%
          filter(st == 1) %>%
          select(lng) %>% as.numeric -> d1
        
        # This is the longitude of the inner side to subdivide triangle
        x <- d1*sin(36*pi/180)/sin(108*pi/180)
        
        # Gradient to calculate point
        df %>%
          filter(st == 2) %>%
          transmute(gx = x - xend,
                    gy = y - yend) %>% as.numeric() -> g2
        
        # Inner point side 2
        df %>%
          filter(st == 2) %>%
          select(xend, yend) %>% as.numeric() + x * g2/sqrt(sum(g2^2)) -> p1
        
        # df1 subtriangle (type 123)
        c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
          p1) %>%
          rbind(c(p1,
                  df %>% filter(st == 2) %>% select(x, y) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 2) %>% select(x, y) %>% as.numeric(),
                  df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df1
        
        colnames(df1) <- c("x", "y", "xend", "yend")
        as_tibble(df1) %>% mutate(st = 1:3) -> df1
        
        # df2 subtriangle (type 124)
        c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
          p1) %>%
          rbind(c(p1,
                  df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric())) %>%
          rbind(c(df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric(),
                  df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df2
        
        colnames(df2) <- c("x", "y", "xend", "yend")
        as_tibble(df2) %>% mutate(st = c(1,4,2)) -> df2
        # Binding two triangles
        bind_rows(df1, df2) %>% round(0)
        
      }
      
      
      Divide <- function(df)
      {
        if(df %>% select(st) %>% sum() %>% as.numeric == 6)
          DivideTriangle123(df)
        else
          DivideTriangle124(df)
      }
      
      
      Divide_df <- function(df){
        df %>%
          mutate(poly_id=rep(1:((nrow(.))/3), each=3)) %>%
          group_by(poly_id) %>%
          group_split() %>% lapply(Divide) %>%  
          bind_rows()
      }
      
      
      Arrange_df <- function(df){
        df %>% round(0) -> df
        
        # Unique points which define segments
        bind_rows(df %>% select(xx = x, yy = y),
                  df %>% select(xx = xend, yy = yend)) %>% unique -> points
        
        # Distance betweem points
        dist_mat <- dist(points, method = 'euclidean')
        hclust_avg <- hclust(dist_mat, method = 'average')
        
        # Group points
        points %>% mutate(group = cutree(hclust_avg, h = 8)) -> points
        
        # Centroid of each cluster
        points %>% group_by(group) %>% summarize(xnew = mean(xx),
                                                 ynew = mean(yy)) -> mean_points
        
        # Substitute each point by its centroid
        points %>% inner_join(mean_points, by = "group") -> points
        
        # Recreate df
        df %>%
          inner_join(points, by = c("x"="xx", "y"="yy")) %>%
          mutate(x = xnew,
                 y = ynew) %>%
          select(-xnew, -ynew) %>%
          left_join(points, by = c("xend"="xx", "yend"="yy")) %>%  
          mutate(xend = xnew,
                 yend = ynew) %>%
          select(-xnew, -ynew) -> df
        
        
        df %>% mutate(poly_id = rep(1:((nrow(.))/3), each=3) %>% as.character) -> df
        
        
        df %>% filter(st %in% c(3,4)) -> df_sub
        
        
        union(df_sub %>%
                inner_join(df_sub, by = c("x","y", "xend", "yend"), suffix = c(".1", ".2")),
              df_sub %>%
                inner_join(df_sub, by = c("x"="xend", "y"="yend","xend"="x", "yend"="y"), suffix = c(".1", ".2"))) %>%
          filter(poly_id.1 != poly_id.2) %>%
          select(poly_id.1, poly_id.2) %>%
          mutate(poly_id_new=apply(., 1, FUN = function (r) sort(r) %>% paste(collapse="_"))) %>%
          gather(condition, poly_id, poly_id.1:poly_id.2) %>%
          select(-condition) %>%
          distinct -> df_new
        
        
        df %>%
          inner_join(df_new, by = "poly_id") %>%
          mutate(remove = st %in% c(3, 4) & !is.na(poly_id_new),
                 poly_id = ifelse(is.na(poly_id_new), poly_id, poly_id_new)) %>%
          filter(remove == FALSE) %>%
          select(-poly_id_new, -remove) %>% unique -> df
        
        
        return(df)
        
      }
      
      
      Create_Polygon <- function(df)
      {
        bind_rows(inner_join(df %>% slice(1),
                             df %>% slice(-1),
                             by = c("xend" = "x", "yend" = "y")) %>%
                    select(x1 = x, y1 = y, x2 = xend, y2 = yend, x3 = xend.y, y3 = yend.y),
                  
                  inner_join(df %>% slice(1),
                             df %>% slice(-1),
                             by = c("xend" = "xend", "yend" = "yend")) %>%
                    select(x1=x.x, y1=y.x, x2=xend, y2=yend, x3 = x.y, y3 = y.y)) -> temp
        
        bind_rows(temp %>% select(x = x1, y = y1),
                  temp %>% select(x = x2, y = y2),
                  temp %>% select(x = x3, y = y3)) -> sub1
        
        bind_rows(df %>% select(x,y),
                  df %>% select(x=xend,y=yend)) %>% unique -> points
        
        bind_rows(sub1,
                  points %>% anti_join(sub1, by = c("x", "y")),
                  sub1 %>% slice(1))
        
      }
      
      
      niter <- depth
      
      init_kite <- CreateKite()
      
      
      seq(from = 0, by=72, length.out = 5) %>%
        lapply(function(x) RotatePolygon(init_kite, angle = x)) %>%
        bind_rows() %>%
        round(0) -> init_polygon
      
      
      
      reduce(
        .x = 1:niter,
        .f = function(old, y) {Divide_df(old)},
        .init=init_polygon) -> raw_tessellation
      
      
      segment_tessellation <- Arrange_df(raw_tessellation)
      
      
      segment_tessellation %>% group_by(poly_id) %>%
        group_split() %>% lapply(Create_Polygon) %>%  
        bind_rows(.id = "poly_id") -> poly_tessellation
      
      
      poly_tessellation %>%
        group_by(poly_id) %>%
        slice(-1) %>%
        mutate(ldx = lead(x, default = first(x)),
               ldy = lead(y, default = first(y))) %>%
        summarize(area=0.5*abs(sum(x*ldy - ldx*y))) -> areas
      
      
      poly_tessellation %>% inner_join(areas, by = "poly_id") -> poly_tessellation
      
      comp_title <- paste("Question 11", compared,"Tesselation depth:", niter)
      # Pick two colours you like and plot it
      plot.comp=ggplot(poly_tessellation) +
        geom_polygon(aes(x=x, y=y, group = poly_id, fill = area),
                     col = "gray65",  
                     lwd = 0,
                     show.legend = NA) +
        scale_fill_gradientn(colours = c(colpal[2], colpal[3])) +
        coord_equal() +
        theme_void() +
        theme(legend.position='none')+theme(plot.background = element_rect(fill = colpal[1], colour = 'red'))+
        ggtitle(comp_title)
      
      #grid.arrange(plot.ref, plot.comp, ncol = 2, widths = c(2, 2), heights = c(2, 2))
      #plot(c(1,2),c(3,4))
      return(list(plot.ref = plot.ref, plot.comp = plot.comp))
    }
    plots <- reactive({
      
      colpal <- character()
      if(input$gender == '2' ) {
        colpal = c("pink")
      } else {
        colpal = c( "lemonchiffon")
      }
      if (input$party == '1'){
        colpal= c(colpal,"steelblue3")
      } else {
        colpal= c(colpal,"brown1")
      }
      if (input$age == '1'){
        colpal= c(colpal,"darkolivegreen3")
      } else {
        colpal= c(colpal,"white")
      }
      
      
      
      
      filtered_data <- tab_df_1 %>% 
        filter(Political.Party == input$party, 
               Gender == input$gender, 
               Age.Category == input$age)
      
      output$perfect_table = renderTable({
        p.t <- filtered_data %>%
          filter(Question == input$reference) %>%
          select(Support, CI.Inverse) 
        
        print(p.t)
      })
      
      output$imperfect_table = renderTable({
        ip.t <- filtered_data %>%
          filter(Question == input$compared) %>%
          select(Support, CI.Inverse) 
        
        print(ip.t)
      })

      
     tesselation.comparison(input$reference,input$compared,input$gender,input$depth, colpal,filtered_data)
     
    })
    
 

      
    output$perfectPlot <- renderPlot({ 
      plots()$plot.ref
    })
    
    output$imperfectPlot <- renderPlot({ 
      plots()$plot.comp
    })
}

# Run the application
shinyApp(ui = ui, server = server)

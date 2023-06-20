k_mean_fn <-
function(data, k_means, seed = NULL){
    if(identical(k_means, "Dob")) {
        k_means_tbl <- data %>% 
            select("Potrošački_Kod", "Dob") %>% 
            scale() %>% 
            kmeans(centers = 4, nstart = 100)
    }
    else if(identical(k_means, "Godišnji_Prihodi")) {
        k_means_tbl <- data %>% 
            select("Potrošački_Kod","Godišnji_Prihodi") %>% 
            scale() %>% 
            kmeans(centers = 5, nstart = 100)
    }
    
    else if(identical(k_means, c("Dob", "Godišnji_Prihodi"))) {
        k_means_tbl <- data %>% 
            select("Potrošački_Kod","Dob", "Godišnji_Prihodi") %>% 
            scale() %>% 
            kmeans(centers = 6, nstart = 100)
    }
    
    return(k_means_tbl)
}
umap_fn <-
function(data, k_means) {
    custom.config <- umap.defaults
    custom.config$random_state <- 123
    if(identical(k_means, "Dob")) {
        umap_obj <- data %>% 
            select("Potrošački_Kod", "Dob") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    else if(identical(k_means, "Godišnji_Prihodi")){
        umap_obj <- data %>% 
            select("Potrošački_Kod","Godišnji_Prihodi") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    else if(identical(k_means, c("Dob", "Godišnji_Prihodi"))){
        umap_obj <- data %>% 
            select("Potrošački_Kod","Godišnji_Prihodi", "Dob") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    
    umap_results_tbl <- umap_obj$layout %>% 
        as_tibble() %>% 
        set_names("x", "y") %>% 
        bind_cols(data %>% select(`ID Kupca`))
    
    return(umap_results_tbl)
    
}


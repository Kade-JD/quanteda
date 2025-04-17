# 1. LOADING SETUP --------------------------------------------------------
library(quanteda)
library(fs)

analysis_dir <- "./data/analysis_objects"

# 2. DIRECTORY VALIDATION -------------------------------------------------
if(!dir_exists(analysis_dir)) {
  dir_create(analysis_dir)
  stop(
    "Analysis directory created at ", analysis_dir, 
    "\nPlease run your analysis pipeline to generate objects."
  )
}

# 3. OBJECT LOADING WITH ERROR HANDLING -----------------------------------
load_analysis_object <- function(object_name) {
  file_path <- path(analysis_dir, paste0(object_name, ".rds"))
  
  if(!file_exists(file_path)) {
    stop("File not found: ", file_path, 
         "\nAvailable files: ", paste(dir_ls(analysis_dir), collapse = "\n"))
  }
  
  tryCatch(
    readRDS(file_path),
    error = function(e) {
      stop("Failed to load ", object_name, ": ", e$message)
    }
  )
}

# 4. SAFE LOADING ---------------------------------------------------------
tokens_obj <- load_analysis_object("tokens")
top_features <- load_analysis_object("top_features")
doc_corpus <- load_analysis_object("corpus")

# 5. OBJECT VALIDATION ----------------------------------------------------
validate_objects <- function() {
  required_classes <- list(
    tokens_obj = "tokens",
    top_features = "numeric",
    doc_corpus = "corpus"
  )
  
  objects <- list(tokens_obj, top_features, doc_corpus)
  names(objects) <- c("tokens_obj", "top_features", "doc_corpus")
  
  for(obj_name in names(objects)) {
    expected_type <- required_classes[[obj_name]]
    
    if(!inherits(objects[[obj_name]], expected_type)) {
      stop("Invalid class for ", obj_name, 
           "\nExpected: ", expected_type, 
           "\nActual: ", class(objects[[obj_name]])[1])
    }
  }
}

validate_objects()

# 6. SUCCESS MESSAGE ------------------------------------------------------
cat(
  "Successfully loaded:\n",
  "- Tokens object (", length(tokens_obj), " documents)\n",
  "- Top features (", length(top_features), " terms)\n",
  "- Corpus (", ndoc(doc_corpus), " documents)",
  sep = ""
)

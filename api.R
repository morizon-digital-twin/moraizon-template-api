#* @apiTitle <Title>
#* @apiDescription <Meaningful description>
#* @apiVersion 1.0.0
#' @plumber
function(pr) {
  
   pr %>% 
    pr_get(
      "/openapi.json",
      function() {
        pr$getApiSpec()
      },
      serializer = plumber::serializer_unboxed_json()
    ) %>%
    pr_get(
      "/__docs__/swagger.json",
      function() {
        pr$getApiSpec()
      },
      serializer = plumber::serializer_unboxed_json()
    ) %>%
    pr_filter("cors", function(res, req) {
      res$setHeader("Access-Control-Allow-Origin", "*")
      res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
      res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With")
      
      if (req$REQUEST_METHOD == "OPTIONS") {
        res$status <- 200
        return(list())
      } else {
        plumber::forward()
      }
    }) %>% 
    #mount rest_controller here ...
    pr_mount("/notfall", plumb("./rest_controller.R")) %>% 
    
    #speficiy api documents here
    pr_set_api_spec(function(spec) {
      
      
      # spec$paths$`/notfall/update_notfall_location`$post$summary <- "Perform a an analysis of emergency facility accessiblity"
      # 
      # 
      # spec$paths$`/notfall/update_notfall_location`$post$requestBody <- list(
      #   description = "Example request body",
      #   required = TRUE,
      #   content = list(
      #     `application/json` = list(
      #       # OpenAPI 3.0 expects 'example' or 'examples' here
      #       schema = list(
      #         type = "object",
      #         example = fromJSON("sample_request_update.json", simplifyVector = T)
      #       )
      #     )
      #   )
      # )
      
      api_url <- getOption(
        "plumber.apiURL",
        Sys.getenv("EXTERNAL_URL", "http://127.0.0.1:8080")
      )
      
      is_local <- file.exists("local_dev/.env_local") ||
        dir.exists("local_dev") ||
        grepl("^https?://(127\\.0\\.0\\.1|localhost)(:|/|$)", api_url)
      
      
      if (is_local) {
        spec$servers <- list(
          list(
            url = api_url,
            description = "Lokaler Entwicklungs-Server"
          ),
          list(
            url = "http://127.0.0.1:8888",
            description = "Lokaler Test-Server"
          )
        )
      } else {
        spec$servers <- list(
          list(
            url = api_url,
            description = "Produktionsserver (extern)"
          )
        )
      }
      # Filter out internal/docs endpoints from the spec
      if (!is.null(spec$paths)) {
        spec$paths <- spec$paths[!grepl("^/(__docs__|__swagger__|openapi\\.json)", names(spec$paths))]
      }

      spec
    })
  
  }
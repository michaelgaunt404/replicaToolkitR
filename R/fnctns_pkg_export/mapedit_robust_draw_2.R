mapedit_robust_draw_2 = function(){
  check_repeat = TRUE


  while (check_repeat) {
    user_created_geometry = tryCatch({
      mapedit::drawFeatures() %>% sf::st_transform(4326)




    }, error = function(e) {
      message("User geometry NA!!!!")
      message(paste("Error occurred with shape generation:\n",
                    e$message))
      message("Likely that you did not press ''Finish'' before pressing ''Done''")
      return(NA)
    })

    if (is.null(user_created_geometry)) {
      check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
    }
    else {
      message("User geometry not NA - good\n")
      check_review = gauntlet::robust_prompt_used("review your geometry (Y) or continue (N)")
      check_repeat = check_review
      if (check_review) {
        mapview::mapview(user_created_geometry) %>%
          print()
        check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
      }
    }

  }
  return(user_created_geometry)
}

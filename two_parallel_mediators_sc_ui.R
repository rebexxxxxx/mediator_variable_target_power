list(
  withTags(
    table(style = "width: 350px;", id = "STDpath_table",
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a1 low")),  
            td(textInput(inputId = "a1_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a1 high")),  
            td(textInput(inputId = "a1_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a2 low")),  
            td(textInput(inputId = "a2_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a2 high")),  
            td(textInput(inputId = "a2_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b1 low")),  
            td(textInput(inputId = "b1_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b1 high")),  
            td(textInput(inputId = "b1_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b2 low")),  
            td(textInput(inputId = "b2_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b2 high")),  
            td(textInput(inputId = "b2_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("c low")),  
            td(textInput(inputId = "c_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("c high")),  
            td(textInput(inputId = "c_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("c low")),  
            td(textInput(inputId = "rm1m2_low", label = NULL, value = "0.14")),
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("c high")),  
            td(textInput(inputId = "rm1m2_high", label = NULL, value = "0.39"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("std. dev: X")),  
            td(textInput(inputId = "SDX", label = NULL, value = "1.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("std. dev: M1")),  
            td(textInput(inputId = "SDM1", label = NULL, value = "1.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("std. dev: M2")),  
            td(textInput(inputId = "SDM2", label = NULL, value = "1.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("std. dev: Y")),  
            td(textInput(inputId = "SDY", label = NULL, value = "1.00"))
          )
    )  
  )
)

library(biogrowth)

server <- function(input, output, session) {

  ## Modules -------------------------------------------------------------------
    
  module_primary_fit_server("module_primary_fit")
  module_primary_pred_server("module_primary_pred")
  module_pred_unc_server("module_pred_uncertainty")
  module_pred_dynamic_server("module_dynamic_pred")
  module_dynamic_fit_server("module_dynamic_fit")
  module_global_fit_server("module_global_fit")
  module_gamma_fit_server("module_gamma_fit")
  module_secondary_fit_server("module_secondary_fit")
  module_other_server("module_other")
  module_twofold_server("module_twofold_dilution")
  module_about_server("module_about")
  module_templates_server("module_templates")

}

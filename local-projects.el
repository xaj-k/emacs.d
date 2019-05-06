;; Create a project for PeepKeep
(ede-cpp-root-project "PeepKeep"
                      :name "PeepKeep"
                      :file '("/home/k-jax/Repositories/PeepKeep")
                      :include-path '("/inc")
                      :targets 'nil
                      :spp-table '(("__cplusplus" . 1))
                      )

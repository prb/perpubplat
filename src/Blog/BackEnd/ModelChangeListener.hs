module Blog.BackEnd.ModelChangeListener where

import Blog.Model.Entry ( Model )

class ModelChangeListener c where
    handle_model_change :: c -> Model -> IO ()

 
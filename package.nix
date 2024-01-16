{ ps-pkgs, pkgs, name, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ css
        halogen
        halogen-css
        halogen-subscriptions
        intersection-observer
        mmorph
        pipes
        resize-observer
        resourcet
      ];
    src = "src";
    pursuit = {
      inherit name; 
      repo = "https://github.com/grybiena/halogen-infinite-scroll.git";
      license = pkgs.lib.licenses.mit;
    };

  }

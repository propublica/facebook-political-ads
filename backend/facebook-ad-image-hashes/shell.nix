let myoverlay = self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper:
        let 
          dj   = self.haskell.lib.doJailbreak;
          dc   = self.haskell.lib.dontCheck;
        in
        {
          phash = null;
          # hs-phash = dj (dc (hself.callPackage ../../../phash/default.nix {}));
          # hs-phash =  ((../../../phash/default.nix));
          hs-phash = hself.callPackage ../../../phash/default.nix { c-phash = pkgs.phash; };
          resourcet = dj (dc hsuper.resourcet_1_1_11);
          postgresql-simple = dj (dc hsuper.postgresql-simple);
        };
      };
    };
    pkgs = import ../../../nixpkgs { overlays = [ myoverlay ]; };
in (pkgs.haskellPackages.callPackage ./. { c-phash = pkgs.phash; }).env
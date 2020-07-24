import (import ./nixpkgs-commit.nix) {
  config = {
    packageOverrides = super: {
      haskellPackages = super.haskellPackages.override {
        overrides = self: super: {
          smtp-mail = self.callCabal2nix "smtp-mail" ./.. {};
          integration-test = self.callCabal2nix "integration-test" ./integration-test {};
        };
      };
    };
  };
}

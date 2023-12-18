let
  # Nixpkgs source to take test environment from.
  nixpkgs-src = import ./nixpkgs-commit.nix;

  # Nixpkgs set with our smtp-mail.
  nixpkgs = import ./nixpkgs.nix;

  # Certificates for ssl in tests.
  certs = import "${nixpkgs-src}/nixos/tests/common/acme/server/snakeoil-certs.nix";

  # Lets use the existing test machinery of nixos, but with our nixpkgs 
  python-test = import "${nixpkgs-src}/nixos/tests/make-test-python.nix";

in
  python-test {
    name = "smtp-mail";

    machine = { pkgs, ... }: {
      imports = [ "${nixpkgs-src}/nixos/tests/common/user-account.nix" ];
      services.postfix = {
        enable = true;
        enableSubmission = true;
        enableSubmissions = true;
        tlsTrustedAuthorities = "${certs.ca.cert}";
        sslCert = "${certs."acme.test".cert}";
        sslKey = "${certs."acme.test".key}";
        submissionOptions = {
          smtpd_sasl_auth_enable = "yes";
          smtpd_client_restrictions = "permit";
          milter_macro_daemon_name = "ORIGINATING";
        };
        submissionsOptions = {
          smtpd_sasl_auth_enable = "yes";
          smtpd_client_restrictions = "permit";
          milter_macro_daemon_name = "ORIGINATING";
        };
      };

      security.pki.certificateFiles = [
        certs.ca.cert
      ];

      networking.extraHosts = ''
        127.0.0.1 acme.test
      '';

      environment.systemPackages = let
      in [ nixpkgs.haskellPackages.integration-test ];
    };

    testScript = ''
      machine.wait_for_unit("postfix.service")
      machine.succeed("integration-test")
    '';
  }

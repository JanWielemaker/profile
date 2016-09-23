# Authentication and authorization library

This library deals with user management  for SWI-Prolog HTTP servers, in
particular:

  - Maintain user profiles: user IDs with properties
  - Authenticate a user based on
    - Local HTTP login
    - Local form/session based login
    - Federated login
      - OpenID
      - oauth
      - SAML
      - ...
  - Perform rule based authorization based on a user ID
    and profile attributes.

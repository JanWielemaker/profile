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

There is a nearly infinite number of choices   to  be made wrt. the used
authentication method, the UI used for  this,   the  way and place where
profile information is stored,  etc.  For   this  reason,  this  library
collection is a plugin system with some default implementations.

## UI

  - If HTTP-based authentication is used the UI is provided by the
    browser and cannot be changed. All we can and need to do is
    challenge the browser.

  - For session-based authentication we must provide a UI.  Depending
    on the authentication method, there are different demands:

    - Federated, generic login (oauth, SAML)
      - Just redirect to the identity provider (IdP)
    - Federated, specific login (OpenID)
      - Ask OpenID and redirect
    - Local form
      - Ask user id and password and verify

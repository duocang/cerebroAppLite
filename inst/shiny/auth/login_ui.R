##----------------------------------------------------------------------------##
## Custom Login UI Module
## A modern, animated login interface for Cerebro
##----------------------------------------------------------------------------##

#' Generate Login Page UI
#' @param welcome_message Custom welcome message to display
#' @return Shiny UI tags for the login page
login_page_ui <- function(welcome_message = "Welcome to Cerebro") {

  tagList(
    # Include custom CSS
    tags$head(
      tags$style(HTML(login_css()))
    ),

    div(
      class = "login-wrapper",

      # Animated background
      div(class = "login-bg"),

      # Login card
      div(
        class = "login-card",

        # Logo/Title section
        div(
          class = "login-header",
          div(class = "login-logo",
            icon("brain", class = "fa-3x")
          ),
          h2(class = "login-title", "Cerebro"),
          div(class = "login-subtitle", HTML(welcome_message))
        ),

        # Login form
        div(
          class = "login-form",

          # Username input
          div(
            class = "input-group",
            tags$label(`for` = "login_username", "Username"),
            div(
              class = "input-wrapper",
              icon("user", class = "input-icon"),
              textInput(
                inputId = "login_username",
                label = NULL,
                placeholder = "Enter your username",
                width = "100%"
              )
            )
          ),

          # Password input
          div(
            class = "input-group",
            tags$label(`for` = "login_password", "Password"),
            div(
              class = "input-wrapper",
              icon("lock", class = "input-icon"),
              passwordInput(
                inputId = "login_password",
                label = NULL,
                placeholder = "Enter your password",
                width = "100%"
              )
            )
          ),

          # Error message
          uiOutput("login_error_msg"),

          # Login button
          div(
            class = "login-btn-wrapper",
            actionButton(
              inputId = "login_btn",
              label = "Sign In",
              class = "login-btn",
              icon = icon("sign-in-alt")
            )
          )
        ),

        # Footer
        div(
          class = "login-footer",
          p("Powered by ", tags$strong("cerebroAppLite"))
        )
      )
    ),

    # JavaScript for Enter key submit
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if (e.which == 13) {
          $('#login_btn').click();
        }
      });
    "))
  )
}


#' Generate CSS for Login Page
#' @return Character string containing CSS
login_css <- function() {
  '
  /* CSS Variables - Deep Blue Theme */
  :root {
    --primary-color: #5b7c99;
    --primary-light: #7a9bb8;
    --primary-dark: #3d5a73;
    --accent-color: #8b9dc3;
    --success-color: #6b9080;
    --warning-color: #e9c46a;
    --danger-color: #e07a5f;
    --background-color: #f7f8fa;
    --card-background: #ffffff;
    --text-primary: #2d3748;
  }

  /* Login Page Styles */
  .login-wrapper {
    min-height: 100vh;
    display: flex;
    align-items: center;
    justify-content: center;
    position: relative;
    overflow: hidden;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
    background: var(--primary-dark);
  }

  /* Water ripple background */
  .login-bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: linear-gradient(180deg, var(--primary-dark) 0%, var(--primary-color) 50%, var(--primary-light) 100%);
    z-index: -2;
  }

  /* Water wave effect */
  .login-bg::before,
  .login-bg::after {
    content: "";
    position: absolute;
    width: 300%;
    height: 300%;
    top: -100%;
    left: -100%;
    background: radial-gradient(ellipse at center, rgba(255,255,255,0.03) 0%, transparent 70%);
    animation: ripple 8s ease-in-out infinite;
  }

  .login-bg::after {
    animation-delay: -4s;
    opacity: 0.5;
  }

  @keyframes ripple {
    0%, 100% {
      transform: translate(0, 0) scale(1);
    }
    25% {
      transform: translate(2%, 2%) scale(1.02);
    }
    50% {
      transform: translate(0, 4%) scale(1);
    }
    75% {
      transform: translate(-2%, 2%) scale(0.98);
    }
  }

  /* Floating wave layers */
  .login-wrapper::before,
  .login-wrapper::after {
    content: "";
    position: fixed;
    width: 200%;
    height: 200%;
    left: -50%;
    border-radius: 40%;
    background: rgba(255, 255, 255, 0.03);
    z-index: -1;
  }

  .login-wrapper::before {
    bottom: -150%;
    animation: wave 15s linear infinite;
  }

  .login-wrapper::after {
    bottom: -155%;
    animation: wave 20s linear infinite reverse;
    opacity: 0.5;
  }

  @keyframes wave {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  /* Login card */
  .login-card {
    background: var(--card-background);
    border-radius: 16px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    padding: 40px;
    width: 100%;
    max-width: 400px;
    margin: 20px;
    animation: slideUp 0.6s ease-out;
    position: relative;
    z-index: 1;
  }

  @keyframes slideUp {
    from {
      opacity: 0;
      transform: translateY(30px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  /* Header section */
  .login-header {
    text-align: center;
    margin-bottom: 30px;
  }

  .login-logo {
    width: 80px;
    height: 80px;
    background: var(--primary-color);
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    margin: 0 auto 20px;
    color: white;
    box-shadow: 0 8px 24px rgba(91, 124, 153, 0.4);
  }

  .login-title {
    color: var(--text-primary);
    font-size: 28px;
    font-weight: 700;
    margin: 0 0 8px 0;
  }

  .login-subtitle {
    color: #718096;
    font-size: 14px;
    margin: 0;
    line-height: 1.5;
  }

  /* Form styles */
  .login-form {
    margin-top: 20px;
  }

  .input-group {
    margin-bottom: 20px;
    width: 100%;
  }

  .input-group label {
    display: block;
    color: var(--text-primary);
    font-size: 13px;
    font-weight: 600;
    margin-bottom: 8px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .input-wrapper {
    position: relative;
    display: flex;
    align-items: center;
    width: 100%;
  }

  .input-icon {
    position: absolute;
    left: 15px;
    top: 50%;
    transform: translateY(-50%);
    color: var(--primary-color);
    z-index: 10;
    pointer-events: none;
  }

  .input-wrapper .form-group {
    margin: 0;
    width: 100%;
  }

  .input-wrapper .shiny-input-container {
    width: 100% !important;
    max-width: 100% !important;
  }

  .input-wrapper input.form-control,
  .input-wrapper input[type="text"],
  .input-wrapper input[type="password"] {
    width: 100% !important;
    max-width: 100% !important;
    padding: 12px 15px 12px 45px !important;
    border: 2px solid #e2e8f0;
    border-radius: 8px;
    font-size: 15px;
    transition: all 0.3s ease;
    background: var(--background-color);
    box-sizing: border-box;
  }

  .input-wrapper input.form-control:focus,
  .input-wrapper input[type="text"]:focus,
  .input-wrapper input[type="password"]:focus {
    border-color: var(--primary-color);
    background: white;
    box-shadow: 0 0 0 3px rgba(91, 124, 153, 0.15);
    outline: none;
  }

  .input-wrapper input.form-control::placeholder,
  .input-wrapper input[type="text"]::placeholder,
  .input-wrapper input[type="password"]::placeholder {
    color: #a0aec0;
  }

  /* Login button */
  .login-btn-wrapper {
    margin-top: 30px;
  }

  .login-btn {
    width: 100%;
    padding: 14px 20px !important;
    background: var(--primary-color) !important;
    border: none !important;
    border-radius: 8px !important;
    color: white !important;
    font-size: 16px !important;
    font-weight: 600 !important;
    cursor: pointer;
    transition: all 0.3s ease !important;
    box-shadow: 0 4px 12px rgba(91, 124, 153, 0.3);
  }

  .login-btn:hover {
    background: var(--primary-dark) !important;
    transform: translateY(-2px);
    box-shadow: 0 6px 16px rgba(91, 124, 153, 0.4) !important;
  }

  .login-btn:active {
    transform: translateY(0);
    background: var(--primary-dark) !important;
  }

  .login-btn .fa, .login-btn .fas {
    margin-right: 8px;
  }

  /* Error message */
  .login-error {
    background: #fef2f2;
    color: var(--danger-color);
    padding: 12px 15px;
    border-radius: 8px;
    font-size: 14px;
    margin-top: 15px;
    display: flex;
    align-items: center;
    animation: shake 0.5s ease-in-out;
    border: 1px solid rgba(224, 122, 95, 0.3);
  }

  .login-error .fa, .login-error .fas {
    margin-right: 10px;
  }

  @keyframes shake {
    0%, 100% { transform: translateX(0); }
    10%, 30%, 50%, 70%, 90% { transform: translateX(-5px); }
    20%, 40%, 60%, 80% { transform: translateX(5px); }
  }

  /* Footer */
  .login-footer {
    text-align: center;
    margin-top: 30px;
    padding-top: 20px;
    border-top: 1px solid #e2e8f0;
  }

  .login-footer p {
    color: #a0aec0;
    font-size: 12px;
    margin: 0;
  }

  /* Loading state */
  .login-btn.loading {
    pointer-events: none;
    opacity: 0.7;
  }

  .login-btn.loading::after {
    content: "";
    width: 16px;
    height: 16px;
    border: 2px solid transparent;
    border-top-color: white;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
    display: inline-block;
    margin-left: 10px;
    vertical-align: middle;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  /* Transition for main app */
  .app-fade-in {
    animation: fadeIn 0.5s ease-out;
  }

  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }

  /* Responsive */
  @media (max-width: 480px) {
    .login-card {
      padding: 30px 20px;
      margin: 10px;
    }

    .login-logo {
      width: 60px;
      height: 60px;
    }

    .login-title {
      font-size: 24px;
    }
  }
  '
}

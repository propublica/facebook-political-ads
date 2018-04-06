import React from "react";
import { connect } from "react-redux";
import { authorize } from "actions.js";

export const LoginUnconnected = ({ authorize }) => {
  let email, password;
  const onLogin = e => {
    e.preventDefault();
    authorize(email.value, password.value);
  };
  return (
    <form id="login" onSubmit={onLogin}>
      <input
        id="email"
        type="text"
        ref={node => (email = node)}
        placeholder="email"
      />
      <input
        id="password"
        type="password"
        ref={node => (password = node)}
        placeholder="password"
      />
      <input id="submit" type="submit" value="login" />
      <br />
      <p>
        Due to a backend change by ProPublica, you may be presented with two
        login screens. Use your same email and password as before for both
        logins. Please email adcollector@propublica.org if your login does not
        work on the other log in screen.
      </p>
    </form>
  );
};
const Login = connect(
  () => ({}),
  dispatch => ({
    authorize: (email, password) => dispatch(authorize(email, password))
  })
)(LoginUnconnected);
export default Login;

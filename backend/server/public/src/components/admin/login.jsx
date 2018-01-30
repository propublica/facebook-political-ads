import React from "react";
import { connect } from "react-redux";
import { authorize } from "actions.js";

let Login = ({ dispatch }) => {
  let email, password;
  const onLogin = e => {
    e.preventDefault();
    dispatch(authorize(email.value, password.value));
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
    </form>
  );
};
export default connect()(Login);

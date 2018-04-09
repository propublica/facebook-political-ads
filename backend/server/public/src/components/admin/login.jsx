import React from "react";
import { connect } from "react-redux";

export const LoginUnconnected = ({}) => {
  return <div />;
};
const Login = connect(() => ({}), dispatch => ({}))(LoginUnconnected);
export default Login;

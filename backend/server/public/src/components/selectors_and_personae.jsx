import React from "react";
import Persona from "components/persona.jsx";
import { connect } from "react-redux";
import { getAds, setPersonaFacet } from "actions.js";

export class SelectorsAndPersonae extends React.Component {
  componentDidUpdate() {
    var el = document.getElementById("sticky_nav");
    var eloffset = el.offsetTop;
    
    window.onscroll = function() {
      var windowTop = window.scrollY;
      if (windowTop > eloffset) {
        el.classList.add('sticky')
      } else {
        el.classList.remove('sticky')
    
      }
    }
  }

  render() {
    return (<div className="selectorsAndPersonae">
      <div className="selectorsOnly">
      <div id="sticky_nav">
        <p>
          Fill out your information to see who is targeting a{" "}
          <input
            type="number"
            value={this.props.persona === null ? "28" : this.props.persona.age}
            onChange={event => {
              this.props.setPersonaFacet("age", event.target.value);
              this.props.getAds();
            }}
          />{" "}
          year old{" "}
          <select
            value={this.props.persona === null ? "" : this.props.persona.gender}
            onChange={event => {
              this.props.setPersonaFacet("gender", event.target.value);
              this.props.getAds();
            }}
          >

            <option>woman</option>
            <option>man</option>
            <option>all genders</option>
          </select>{" "}
          of any race living in{" "}
          <select
            value={this.props.persona === null ? "" : this.props.persona.location[0]}
            onChange={event => {
              this.props.setPersonaFacet("location", [event.target.value, null]);
              this.props.getAds();
            }}
          >
            <option>Alabama</option>
            <option>Alaska</option>
            <option>Arizona</option>
            <option>Arkansas</option>
            <option>California</option>
            <option>Colorado</option>
            <option>Connecticut</option>
            <option>Delaware</option>
            <option>District of Columbia</option>
            <option>Florida</option>
            <option>Georgia</option>
            <option>Hawaii</option>
            <option>Idaho</option>
            <option>Illinois</option>
            <option>Indiana</option>
            <option>Iowa</option>
            <option>Kansas</option>
            <option>Kentucky</option>
            <option>Louisiana</option>
            <option>Maine</option>
            <option>Maryland</option>
            <option>Massachusetts</option>
            <option>Michigan</option>
            <option>Minnesota</option>
            <option>Mississippi</option>
            <option>Missouri</option>
            <option>Montana</option>
            <option>Nebraska</option>
            <option>Nevada</option>
            <option>New Hampshire</option>
            <option>New Jersey</option>
            <option>New Mexico</option>
            <option>New York</option>
            <option>North Carolina</option>
            <option>North Dakota</option>
            <option>Ohio</option>
            <option>Oklahoma</option>
            <option>Oregon</option>
            <option>Pennsylvania</option>
            <option>Puerto Rico</option>
            <option>Rhode Island</option>
            <option>South Carolina</option>
            <option>South Dakota</option>
            <option>Tennessee</option>
            <option>Texas</option>
            <option>Utah</option>
            <option>Vermont</option>
            <option>Virginia</option>
            <option>Washington</option>
            <option>West Virginia</option>
            <option>Wisconsin</option>
            <option>Wyoming</option>
          </select>{" "}
          and who is{" "}
          <select
            value={this.props.persona === null ? "" : this.props.persona.politics}
            onChange={event => {
              this.props.setPersonaFacet("politics", event.target.value);
              this.props.getAds();
            }}
          >
            <option>conservative</option>
            <option>liberal</option>
            <option>apolitical</option>
          </select>.
        </p>
        </div>
        </div>
        <div className="race-note">
          <p>
            Note: We don't let you filter by race because few political advertisers target
            using Facebook’s “multicultural affinity” category.
          </p>
        </div>
      
      <div>

        <p>Or see who is targeting a:</p>

        <ul className="flexy">
          {[
            {
              age: "72",
              gender: "man",
              politics: "conservative",
              location: ["District of Columbia", "Washington"],
              pretty_location: "Washington, D.C.",
              img: "man-65"
            },
            {
              age: "40",
              gender: "woman",
              politics: "liberal",
              location: ["California"],
              pretty_location: "California",
              img: "woman-40"
            },
            {
              age: "20",
              gender: "man",
              politics: "apolitical",
              location: ["Wisconsin"],
              pretty_location: "Wisconsin",
              img: "man-20"
            },
            {
              age: "32",
              gender: "woman",
              politics: "conservative",
              location: ["Texas"],
              pretty_location: "Texas",
              img: "woman-32"
            }
          ].map(persona => (
            <li key={persona.img} className="quarter">
              <Persona persona={persona} />
            </li>
          ))}
        </ul>
      </div>
    </div>);
  }
}

export default connect(
  ({ persona }) => ({ persona }),
  dispatch => ({
    getAds: () => dispatch(getAds()),
    setPersonaFacet: (facet_key, facet_val) =>
      dispatch(setPersonaFacet(facet_key, facet_val))
  })
)(SelectorsAndPersonae);

import React from "react";
import { Provider } from "react-redux";
import renderer from "react-test-renderer";
import { Ad, RatingForm } from "../../src/components/ads.jsx";
import { getMessage } from "../../src/i18n.js";
import { RatingType } from "../../src/constants.js";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import configureMockStore from "redux-mock-store";

Enzyme.configure({ adapter: new Adapter() });

const mockStore = configureMockStore();
const ads = JSON.parse(require("fs").readFileSync(__dirname + "/../ads.json"));

describe("ads", () => {
  it("should render an ad", () => {
    const ad = renderer.create(<Ad id="1" html="<b>Hello!</b>" />);
    expect(ad.toJSON()).toMatchSnapshot();
  });

  it("should show a rating form", () => {
    const action = (rating, type) => {
      expect(rating).toBe(ads.ads[0]);
      expect([RatingType.POLITICAL, RatingType.NORMAL]).toContain(type);
    };

    const form = mount(
      <Provider store={mockStore({ language: "en" })}>
        <RatingForm
          getMessage={getMessage}
          rating={ads.ads[0]}
          action={action}
          question="verify_question"
        />
      </Provider>
    );
    expect(form).toMatchSnapshot();
    form.find("button").forEach(node => node.simulate("click"));
  });
});

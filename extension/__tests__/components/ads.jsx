import React from "react";
import { Provider } from "react-redux";
import renderer from "react-test-renderer";
import {
  Ad,
  Ads,
  RatingForm,
  UnratedRatings
} from "../../src/components/ads.jsx";
import { getMessage } from "../../src/i18n.js";
import { RatingType } from "../../src/constants.js";
import { updateRating, updateAd } from "../../src/actions.js";
import { mount } from "enzyme";
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";

const mockStore = configureMockStore([thunk]);
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

  it("should display ratings", () => {
    const store = mockStore({ language: "en", ratings: ads.ads });
    const ratings = mount(
      <Provider store={store}>
        <UnratedRatings ratings={ads.ads} />
      </Provider>
    );
    expect(ratings).toMatchSnapshot();
    ratings
      .find("button")
      .at(0)
      .simulate("click");
    expect(store.getActions()[0]).toEqual(
      updateRating(ads.ads[0].id, RatingType.POLITICAL)
    );
  });

  it("should display ads", () => {
    ads.ads[1].rating = RatingType.POLITICAL;
    const store = mockStore({ language: "en", ads: ads.ads });
    const ratings = mount(
      <Provider store={store}>
        <Ads ratings={ads.ads} />
      </Provider>
    );
    expect(ratings).toMatchSnapshot();
    ratings
      .find("button")
      .at(0)
      .simulate("click");
    expect(store.getActions()[0]).toEqual(
      updateAd(ads.ads[0].id, RatingType.POLITICAL)
    );
  });
});

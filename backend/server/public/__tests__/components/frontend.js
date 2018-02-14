import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { FrontendUnconnected } from "../../src/components/frontend.jsx";
Enzyme.configure({ adapter: new Adapter() });
import { go } from "i18n.js";

function setup({ ads }) {
  const props = {
    ads
  };

  const enzymeWrapper = shallow(<FrontendUnconnected {...props} />, {
    disableLifecycleMethods: true
  });

  return {
    props,
    enzymeWrapper
  };
}
describe("components", () => {
  describe("FrontendUnconnected", () => {
    it("should not render any ads if it has no ads in props", () => {
      go(() => {
        const { enzymeWrapper } = setup({
          ads: []
        });
        expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(
          0
        );
      });
    });
    it("should render as many ads as are in props", () => {
      go(() => {
        const { enzymeWrapper } = setup({
          ads: [{ id: 1 }, { id: 2 }]
        });
        expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(
          2
        );
      });
    });

    it("should not render pagination if there are zero ads", () => {
      go(() => {
        const { enzymeWrapper } = setup({
          ads: []
        });
        expect(enzymeWrapper.find("Connect(Pagination)")).toHaveLength(0);
      });
    });

    it("should render pagination if there are >=1 ads", () => {
      go(() => {
        const { enzymeWrapper } = setup({ ads: [{ id: 1 }, { id: 2 }] });
        expect(enzymeWrapper.find("Connect(Pagination)")).toHaveLength(1);
      });
    });

    it("should render at least one Term", () => {
      go(() => {
        const { enzymeWrapper } = setup({
          ads: [{ id: 1 }, { id: 2 }]
        });
        expect(enzymeWrapper.find("Connect(Term)").exists()).toBe(true);
      });
    });
  });
});

import React from "react";
import { Collapse } from "react-collapse";

class BottomGuffItem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      shown: props.shown
    };
  }
  render() {
    return (
      <div className="bottom-guff">
        <h3
          dangerouslySetInnerHTML={{
            __html:
              this.props.titleHtml +
              (this.state.shown
                ? " <span class=\"collapse-handle\">&ndash;</span>"
                : " <span class=\"collapse-handle\">+</span>")
          }}
          onClick={() => this.setState({ shown: !this.state.shown })}
        />
        <Collapse isOpened={this.state.shown}>
          <div dangerouslySetInnerHTML={{ __html: this.props.bodyHtml }} />
        </Collapse>
      </div>
    );
  }
}

export default BottomGuffItem;

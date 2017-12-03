import { h } from 'preact';
import { connect } from 'preact-redux';
import { t } from 'i18n.js';

const Filter = ({ data, title }) => (
  <div className="filter">
    <h3 className="filter-title">{title} (0/4)</h3>

  </div>
);
let Filters = ({ entities, advertisers, targets }) => (
  <div className="filters">
    <Filter data={entities} title={t("related_terms")} />
    <Filter data={advertisers} title={t("advertiser")}/>
    <Filter data={targets} title={t("target_audience")}/>
  </div>
);
Filters = connect(
  (state) => ({
    entities: state.entities,
    advertisers: state.advertisers,
    targets: state.targets
  }),
  () => ({})
)(Filters);

export { Filters };

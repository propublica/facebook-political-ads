import { h } from 'preact';
import { connect } from 'preact-redux';
import { t } from 'i18n.js';
import { uniqWith, isEqual } from 'lodash';

const NEW_ENTITIES = 'new_entities';
const NEW_ADVERTISERS = 'new_advertisers';
const NEW_TARGETS = 'new_targets';
const FILTER_ENTITY = 'filter_entity';
const FILTER_ADVERTISER = 'filter_advertiser';
const FILTER_TARGET = 'filter_target';

const a = (type) => ((arg) => ({ type, value: arg }));
const newEntities = a(NEW_ENTITIES);
const newAdvertisers = a(NEW_ADVERTISERS);
const newTargets = a(NEW_TARGETS);
const filterEntity = a(FILTER_ENTITY);
const filterAdvertiser = a(FILTER_ADVERTISER);
const filterTarget = a(FILTER_TARGET);


const makeReducer = (plural, singular) => {
  return (state = { available: [], filtered: []}, action) => {
    switch(action.type) {
    case `new_${plural}`:
      return {
        ...state,
        available: action.value.map((filter) => ({
          ...filter,
          key: filter[singular],
          active: state.filtered.filter((it) => it[singular] === filter[singular]).length > 0
        }))
      };
    case `filter_${plural}`:
      return {
        ...state,
        filtered: uniqWith(state.filtered.concat([action.value]), isEqual)
      };
    default:
      return state;
    }
  };
};

const entities = makeReducer("entities", "entity");
const advertisers = makeReducer("advertisers", "advertiser");
const targets = makeReducer("targets", "target");

const Filter = ({ data, title, activate }) => (
  <div className="filter">
    <h3 className="filter-title">{title}</h3>
    {data.map((filter) => <li key={filter.key + "-li"}>
      <input
        type="checkbox"
        name={filter.key}
        checked={filter.active}
        key={filter.key}
        onChange={() => activate(filter)} />
      <label htmlFor={filter.key}>{filter.key} ({filter.count})</label>
    </li>)}
  </div>
);

let Filters = ({ entities, advertisers, targets, dispatch }) => (
  <div className="filters">
    <Filter
      data={entities.available}
      title={t("related_terms")}
      activate={(it) => dispatch(filterEntity(it)) } />
    <Filter
      data={advertisers.available}
      title={t("advertiser")}
      activate={(it) => dispatch(filterAdvertiser(it)) } />
    <Filter
      data={targets.available}
      title={t("target_audience")}
      activate={(it) => dispatch(filterTarget(it)) } />
  </div>
);
Filters = connect(
  ({ entities, advertisers, targets }) => ({ entities, advertisers, targets }),
)(Filters);

export { Filters, entities, advertisers, targets, newEntities, newAdvertisers, newTargets };

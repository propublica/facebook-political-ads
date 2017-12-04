import { h } from 'preact';
import { connect } from 'preact-redux';
import { t } from 'i18n.js';

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

const TOGGLE_TARGET = 'toggle_target';
const TOGGLE_ADVERTISER = 'toggle_advertiser';
const TOGGLE_ENTITY = 'toggle_entity';

const toggleTarget = () => ({ type: TOGGLE_TARGET });
const toggleAdvertiser = () => ({ type: TOGGLE_ADVERTISER });
const toggleEntity = () => ({ type: TOGGLE_ENTITY });

const filters = (state = {}, action) => {
  switch(action.type) {
  case TOGGLE_TARGET:
    return { ...state, target: !state.target };
  case TOGGLE_ADVERTISER:
    return { ...state, advertiser: !state.advertiser };
  case TOGGLE_ENTITY:
    return { ...state, entity: !state.entity };
  default:
    return state;
  }
};

const makeReducer = (plural, singular) => {
  return (state = [], action) => {
    switch(action.type) {
    case `new_${plural}`: {
      const lookup = new Set(state.filter((filter) => filter.active).map((it) => it[singular]));
      return action.value.map((filter) => ({
        ...filter,
        key: filter[singular],
        active: lookup.has(filter[singular])
      }));
    }
    case `filter_${singular}`:
      return state.map((filter) => {
        if(filter[singular] === action.value[singular]) {
          return {
            ...filter,
            active: !filter.active
          };
        } else {
          return filter;
        }
      });
    default:
      return state;
    }
  };
};

const entities = makeReducer("entities", "entity");
const advertisers = makeReducer("advertisers", "advertiser");
const targets = makeReducer("targets", "target");

const s = (plural, singular, map) => {
  return (params, state) => {
    const items = state[plural]
      .filter((it) => it.active)
      .map((it) => map ? map(it[singular]) : it[singular]);
    if(items.length > 0) {
      params.set(plural, JSON.stringify(items));
    }
    return params;
  };
};

const serializeEntities = s("entities", "entity", (entity) => ({ entity }));
const serializeAdvertisers = s("advertisers", "advertiser");
const serializeTargets = s("targets", "target", (target) => ({ target }));

const Filter = ({ data, title, activate, toggle, active }) => (
  <div className={ active ? "active filter" : "filter" }>
    <h3 className="filter-title" onClick={toggle}>{title}</h3>
    <fieldset className="filter-options">
      <ul>
        {data.map((filter) => <li key={filter.key + "-li"}>
          <input
            type="checkbox"
            name={filter.key}
            checked={filter.active}
            key={filter.key}
            onChange={() => activate(filter)} />
          <label
            htmlFor={filter.key}
            onClick={ () => activate(filter)}>{filter.key} ({filter.count})</label>
        </li>)}
      </ul>
    </fieldset>
  </div>
);


let Filters = ({ entities, advertisers, targets, filters, dispatch }) => (
  <div className="filters">
    <Filter
      data={entities}
      title={t("related_terms")}
      activate={(it) => dispatch(filterEntity(it))}
      toggle={() => dispatch(toggleEntity())}
      active={filters.entity} />
    <Filter
      data={advertisers}
      title={t("advertiser")}
      activate={(it) => dispatch(filterAdvertiser(it)) }
      toggle={() => dispatch(toggleAdvertiser())}
      active={filters.advertiser} />
    <Filter
      data={targets}
      title={t("target_audience")}
      activate={(it) => dispatch(filterTarget(it))}
      toggle={() => dispatch(toggleTarget())}
      active={filters.target} />
  </div>
);
Filters = connect(
  ({ entities, advertisers, targets, filters }) => ({
    entities,
    advertisers,
    targets,
    filters
  }),
)(Filters);

export {
  Filters, entities, advertisers, targets,
  newEntities, newAdvertisers, newTargets,
  serializeAdvertisers, serializeTargets, serializeEntities,
  filterAdvertiser, filterEntity, filterTarget, filters
};

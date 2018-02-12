import React from "react";
import { connect } from "react-redux";
import { t } from "i18n.js";
import {
  filterEntity,
  toggleEntity,
  filterAdvertiser,
  toggleAdvertiser,
  filterTarget,
  toggleTarget
} from "actions.js";

const Filter = ({ data, title, activate, toggle, active }) => (
  <div className={active ? "active filter" : "filter"}>
    <h3 className="filter-title" onClick={toggle}>
      {title}
    </h3>
    <fieldset className="filter-options">
      <ul>
        {data.map(filter => (
          <li key={filter.key + "-li"}>
            <input
              type="checkbox"
              name={filter.key}
              checked={filter.active}
              key={filter.key}
              onChange={() => activate(filter)}
            />
            <label htmlFor={filter.key} onClick={() => activate(filter)}>
              {t(filter.key)} ({filter.count})
            </label>
          </li>
        ))}
      </ul>
    </fieldset>
  </div>
);

const FiltersUnconnected = ({
  entities,
  advertisers,
  targets,
  filters,
  filterEntity,
  toggleEntity,
  filterAdvertiser,
  toggleAdvertiser,
  filterTarget,
  toggleTarget
}) => (
  <div className="filters">
    <Filter
      data={entities}
      title={t("related_terms")}
      activate={it => filterEntity(it)}
      toggle={() => toggleEntity()}
      active={filters.entity}
    />
    <Filter
      data={advertisers}
      title={t("advertiser")}
      activate={it => filterAdvertiser(it)}
      toggle={() => toggleAdvertiser()}
      active={filters.advertiser}
    />
    <Filter
      data={targets}
      title={t("target_audience")}
      activate={it => filterTarget(it)}
      toggle={() => toggleTarget()}
      active={filters.target}
    />
  </div>
);
const Filters = connect(
  ({ entities, advertisers, targets, filters }) => ({
    entities,
    advertisers,
    targets,
    filters
  }),
  dispatch => ({
    filterEntity: item => dispatch(filterEntity(item)),
    toggleEntity: () => dispatch(toggleEntity()),
    filterAdvertiser: item => dispatch(filterAdvertiser(item)),
    toggleAdvertiser: () => dispatch(toggleAdvertiser()),
    filterTarget: item => dispatch(filterTarget(item)),
    toggleTarget: () => dispatch(toggleTarget())
  })
)(FiltersUnconnected);

export { Filter, FiltersUnconnected, Filters };

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

let Filters = ({ entities, advertisers, targets, filters, dispatch }) => (
  <div className="filters">
    <Filter
      data={entities}
      title={t("related_terms")}
      activate={it => dispatch(filterEntity(it))}
      toggle={() => dispatch(toggleEntity())}
      active={filters.entity}
    />
    <Filter
      data={advertisers}
      title={t("advertiser")}
      activate={it => dispatch(filterAdvertiser(it))}
      toggle={() => dispatch(toggleAdvertiser())}
      active={filters.advertiser}
    />
    <Filter
      data={targets}
      title={t("target_audience")}
      activate={it => dispatch(filterTarget(it))}
      toggle={() => dispatch(toggleTarget())}
      active={filters.target}
    />
  </div>
);
Filters = connect(({ entities, advertisers, targets, filters }) => ({
  entities,
  advertisers,
  targets,
  filters
}))(Filters);

export { Filter, Filters };

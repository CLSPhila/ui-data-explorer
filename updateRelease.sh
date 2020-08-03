#!/bin/bash


# note - to run using env vars in the running shell,run with `. .updateRelease`

# Arguments with defaults.
owner="${1:-clsphila}" # owner of the repository we're updating.
repository="${2:-ui-data-explorer}" # the name of the repo we're updating.
release_name="${3:-uiExplorerData}" # Name of the release we're updating/
asset_name="${4:-unemployment_data.parquet}" # Name of the asset we're updating. 
data_dir="${5:-data}" # Directory where the asset exists locally to this script.

if [ "$GITHUB_TOKEN" == "" ]; then
  echo "Expected github token missing. Have you set up environment variables?"
  return 1
fi

echo "----------"
echo "Updating $owner/$repository, release '$release_name' with the asset '$asset_name'."

function upload_asset {
  release_id=$1
  echo "  Uploading asset to release '$release_id'."
  uploaded_response=$(curl \
    -X POST \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept:application/vnd.github.v3+json" \
    -H "Content-Type:application/octet-stream" \
    --data-binary @"$data_dir/$asset_name" \
    "https://uploads.github.com/repos/$owner/$repository/releases/$release_id/assets?name=$asset_name"
    )
  echo "  Finished uploading asset."
  echo "  $uploaded_response"
  echo "--------------------------"
}

function create_release {
  echo "Release not found. Creating."
  new_release=$(curl \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
     "https://api.github.com/repos/$owner/$repository/releases" \
    -d "{\"tag_name\":\"$release_name\", \"name\":\"$release_name\"}"
    )
  echo "new release: $new_release"
  release_id=$(jq '.id' <<< $new_release)
  echo "Assets url: $assets_url"
  echo "end of assets url"
  upload_asset $release_id
}

function clear_assets {
  assets=$1
  for row in $(echo "${assets}" | jq -r '.[] | @base64'); do
    _jq() {
     echo ${row} | base64 --decode | jq -r ${1}
    }
    asset_id="$(_jq '.id')"
    echo "Delete Asset: $asset_id"
    deleted=$(curl \
    -X DELETE \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
     "https://api.github.com/repos/$owner/$repository/releases/assets/$asset_id"
    )
  done 
}

function replace_asset {
  release=$1
  release_id=$(jq '.id' <<< "$release")
  echo "  Replacing assets for the release $release_id."
  assets=$(jq '.assets' <<< "$release")
  asset_count=$(jq '. | length'  <<< "$assets")  
  echo "  The release has $asset_count assets."
  if [ $asset_count != "0" ]; then
    echo "  We need to delete assets before uploading."
    clear_assets $assets
  else:
    echo "  We do not need to delete any assets before uploading."
  fi

  assets_url=$(jq '.assets_url' <<< "$release")
  release_id=$(jq '.id' <<< "$release")
  upload_asset $release_id
}

# get the releases
releases_complete=$(curl \
  -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/clsphila/ui-data-explorer/releases)


# find the release we care about with its name.
releases=$(jq --arg release_name "$release_name" '[.[] | select(.name==$release_name) | {id: .id, upload_url: .upload_url, assets_url: .assets_url, name: .name, assets: .assets}]'  <<< "$releases_complete")


# if this release doesn't exist, create it and upload the asset.
length=$(jq '. | length' <<< "$releases")

echo "  We found $length releases named '$release_name' already published."


if [ $length == "0" ];
then
  create_release
else
  if [ $length == "1" ];
  then
    # There should only be 1 release with the name $release_name. 
    release=$(jq '.[0]' <<< "$releases")
    replace_asset "$release"
  else
    echo "More than one release named $release_name found. Not sure what to do. Exiting." 
    return 1
  fi
fi


echo "---COMPLETED---"
   


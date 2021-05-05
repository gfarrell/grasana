import { env, isLocalEnv } from "./utils";

export const token = isLocalEnv() ? "12345" : env("ASANA_PAT");
